defmodule SupervisorProcess do
  # A Supervisor is a process that supervises other processes/supervised processes and restarts them whenever they crash
  # by supervision we mean 3 things 1. Starting 2. Stopping 3. Restarting

  # Once the supervisor starts, it will traverse the list of children and it will invoke the child_spec/1
  # child_spec/1 returns the child specification describes how to start the process, if the process is a worker, supervisor, if temporary, transient or permanent.
  use Agent
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      # We can pass Module names(since modules identifiers are atoms) instead of PIDs to the supervisor
      {Test, name: Test}
    ]

    # strategy dictates what happens when one of the children crashes
    Supervisor.init(children, strategy: :one_for_one)
    # :one_for_one means that if a child dies, it will be the only one restarted
  end
end

# {:ok,sup}=SupervisorProcess.start_link([]) this will start the supervisor process and its child Test
# [{_, registry, _, _}] =Supervisor.which_children(sup) this will show #[{Test, #PID<0.120.0>, :worker, [Test]}]
# GenServer.call(registry, :bad_input)  this will only crush the process registry
# if we hit which_children after crushing the child process (registry)we will find that the registry working with new pid(this means he got rebooted)

# Now we can use the process name instead of the PID
# Test.create(Test, "shopping")
# Test.lookup(Test,"shopping")
defmodule Test do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:lookup, name}, _from, names) do
    {:reply, Map.fetch(names, name), names}
  end

  @impl true
  def handle_cast({:create, name}, names) do
    if Map.has_key?(names, name) do
      {:noreply, names}
    else
      # notice here that every bucket associate to the registry which means every bucket crush will crush whole registry
      #  crashing the registry still means we lose all data associating bucket, this will be solved in DynamicSupervisor
      {:ok, bucket} = start_link([])
      {:noreply, Map.put(names, name, bucket)}
    end
  end
end
