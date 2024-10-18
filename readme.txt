the only benifit for gen_server_registry over agents is the monitoring of buckets
i created gen_server_registry.ex which is without monitoring to facilitate understanding the code responsible of monitoring in gen_server_registry_with_monitoring.ex
________________
ETS (Erlang Term Storage) is a cache mechanism nut 
Registry = GenServer + :ets
________________
to fix some concurrency problem  :
assert KV.Registry.lookup(registry, "shopping") is being processed before handle_info gets the :DOWN message,
Agent.stop/2 operation is synchronous but it may not have processed the :DOWN message yet

Since messages are processed in order, once the registry replies to the synchronous "bogus" request, then the :DOWN message will definitely have been processed.
  test "removes bucket on crash", %{registry: registry} do
    KV.Registry.create(registry, "shopping")
    {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

    # sending a synchronous request to the registry
    Agent.stop(bucket, :shutdown)

    # Do a call to ensure the registry processed the DOWN message
    _ = KV.Registry.create(registry, "bogus")
    assert KV.Registry.lookup(registry, "shopping") == :error
  end
