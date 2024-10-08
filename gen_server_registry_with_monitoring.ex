defmodule GenServerRegistry do
  use GenServer
  # this is the same as the gen_server_registry.ex one but with monitoring
  # we changed the server without changing the client API.

  # Links are bi-directional. If you link two processes and one of them crashes,the other  will crash too
  # A monitor is uni-directional: only the monitoring process will receive notifications about the monitored one

  # Defining GenServer client OR client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end

  # Defining GenServer Callbacks(server)
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
      {:ok, bucket} = start_link([])
      ref = Process.monitor(bucket)
      refs = Map.put(refs, ref, name)
      names = Map.put(names, name, bucket)
      {:noreply, {names, refs}}
    end
  end

  # handle_info for monitoring
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

# Calling this will create a bucket process and return the {:ok , pid}
# {:ok, registry} = GenServerRegistry.start_link([])
# GenServerRegistry.create(registry, "shopping") this will create a bucket
# {:ok, bucket} = GenServerRegistry.lookup(registry, "shopping")
# _____________________________
# Agent.stop(bucket) # Agent.stop(registry) wont give u feedback because we attend to monitor registry buckets not the parent registry
# Process.monitor(registry) #registry is the registry process pid if we give it to the monitor functinon it will return reference
# we can use the ref value to match against the process mailbox after using flush()
# flush()
