# defmodule DynamicSupervisorT do

#   use Agent
#   use DynamicSupervisor

#   def start_link(_init_arg) do
#     children = [
#       {DynamicSupervisorT, name: DynamicSupervisorT, strategy: :one_for_one}
#     ]
#     DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)

#   end

#   @impl true
#   def init(_init_arg) do
#     DynamicSupervisor.init(strategy: :one_for_one)
#   end
# end

defmodule DynamicSupervisorT do
  # in supervisor_process we used link "start_link" in handle_cast callback
  # every bucket associate to the registry which means every bucket crush will crush whole registry
  # crashing the registry still means we lose all data associating bucket
  # _________________
  # We have to be carful about order of children and the strategies
  # _________________

  # in DynamicSupervisor does not expect a list of children during initialization coz the children are not known upfront,
  # but they are rather started dynamically on demand, also module file is not required

  use Agent
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      # notice there s no BucketSupervisor module no need for declaring the supervisor module in the dynamic supervisor
      {DynamicSupervisor, name: BucketSupervisor, strategy: :one_for_one},
      {Registry, name: Registry}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# {:ok,sup}=DynamicSupervisorT.start_link([]) we lunch the app
# {:ok, bucket_supervisor1} = DynamicSupervisor.start_child(BucketSupervisor, Bucket) we make Bucket a child of BucketSupervisor

defmodule Registry do
  use Supervisor

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end

  # _________________
  @impl true
  def init(:ok) do
    names = %{}
    refs = %{}
    {:ok, {names, refs}}
  end

  @impl true
  def handle_call({:lookup, name}, _from, state) do
    {names, _} = state

    {:reply, Map.fetch(names, name), state}
  end

  @impl true
  def handle_cast({:create, name}, {names, refs}) do
    if Map.has_key?(names, name) do
      {:noreply, {names, refs}}
    else
      {:ok, bucket} = DynamicSupervisor.start_child(BucketSupervisor, Bucket)

      ref = Process.monitor(bucket)
      refs = Map.put(refs, ref, name)
      names = Map.put(names, name, bucket)
      {:noreply, {names, refs}}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
    IO.puts("handle_info")
    {name, refs} = Map.pop(refs, ref)
    names = Map.delete(names, name)
    {:noreply, {names, refs}}
  end

  @impl true
  def handle_info(msg, state) do
    IO.puts("Unexpected message in GenServerRegistry: #{inspect(msg)}")

    {:noreply, state}
  end
end

defmodule Bucket do
  use Agent, restart: :temporary
  use Supervisor

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:create, name}, {names, refs}) do
    if Map.has_key?(names, name) do
      {:noreply, {names, refs}}
    else
      # instead of start_link DynamicSupervisor.start_child now when bucket crushes the registry wont
      # But also it created memory leak, When a bucket terminates the supervisor will start a new bucket in its place
      # and the registry wont know about it, so we cant access it,right thing that bucket should never restarted

      # why use a supervisor if it never restarts its children.Supervisors provide more than restarts
      # they are responsible for guaranteeing proper startup and shutdown, especially in case of crashes in a supervision tree.

      {:ok, bucket} = DynamicSupervisor.start_child(BucketSupervisor, Bucket)

      ref = Process.monitor(bucket)
      refs = Map.put(refs, ref, name)
      names = Map.put(names, name, bucket)
      {:noreply, {names, refs}}
    end
  end
end
