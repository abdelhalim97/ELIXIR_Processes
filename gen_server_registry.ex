defmodule GenServerRegistry do
  use GenServer

  # the registry needs to monitor each bucket
  # The registry needs to guarantee that it is always up to date.
  # For example, if one of the bucket processes crashes due to a bug, the registry must notice this change and avoid serving stale entries

  # GenServer can create a registry process that can monitor the bucket processes

  # the client and server part in agents together but in gen_server will be separate

  # Defining GenServer client OR client API

  @doc """
  Starts the registry.
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @doc """
  Looks up the bucket pid for `name` stored in `server`.

  Returns `{:ok, pid}` if the bucket exists, `:error` otherwise.
  """
  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  @doc """
  Ensures there is a bucket associated with the given `name` in `server`.
  """
  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end

  # Defining GenServer Callbacks(server)
  # @impl for callbacks
  # for example when GenServer.cast gets called handle_cast will be called too as a response(callback)

  # start_link/3 or start/3 will block until it returns.
  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  # calls r sync and client will wait the server response
  @impl true
  def handle_call({:lookup, name}, _from, names) do
    {:reply, Map.fetch(names, name), names}
  end

  # cast are async and server wont send response and so client, instead the server will immediately return
  @impl true
  def handle_cast({:create, name}, names) do
    if Map.has_key?(names, name) do
      {:noreply, names}
    else
      {:ok, bucket} = start_link([])
      {:noreply, Map.put(names, name, bucket)}
    end
  end
end

# Calling this will create a bucket process and return the {:ok , pid}
# {:ok, registry} = GenServerRegistry.start_link([])
# GenServerRegistry.create(registry,  "shopping")
# {:ok, bk} = GenServerRegistry.lookup(registry, "shopping")
# _____________________________
# Agent.stop(bk) // the linked process(registry) wont crush even both r linked coz stop send :normal is a cleans shutdown
# Process.monitor(registry) #registry is the registry process pid if we give it to the monitor functinon it will return reference
# we can use the ref value to match against the process mailbox after using flush()
# flush()

# even after stoping using Agent.stop(bk) the bucket the bucket name will remain in the registry by using lookup,
# will address this problem with monitoring functions in gen_server_registry_with_monitoring.ex
