defmodule Agents do
  # Agents are simple wrappers around state. If all you want from a process is to keep state, agents are a great fit
  # When a long action is performed on the server, all other requests to that particular server will wait until the action is done
  use Agent

  def start_link() do
    Agent.start_link(fn -> %{} end)
  end

  def get(bucket, key) do
    Agent.get(bucket, &Map.get(&1, key))
  end

  def put(bucket, key, value) do
    Agent.update(bucket, &Map.put(&1, key, value))
  end

  def delete(bucket, key, value) do
    # Here is the client code
    Process.sleep(500)

    Agent.update(bucket, fn state ->
      Map.pop(state, key)
      # Here is the server code
      Process.sleep(1000)
    end)

    # Back to the client code
  end
end
