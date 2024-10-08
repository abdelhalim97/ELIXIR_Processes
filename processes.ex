defmodule MyProcess do
  def awaiting_for_receive_messages(messages \\ []) do
    IO.inspect(self())

    receive do
      "Hi" = msg ->
        IO.puts("Hello")

        [msg | messages]
        |> IO.inspect()
        |> awaiting_for_receive_messages()

      msg ->
        IO.puts("default")

        [msg | messages]
        |> IO.inspect()
        |> awaiting_for_receive_messages()
    end

    # if we dont recall the fnc it will terminated after we send message
    awaiting_for_receive_messages()
  end

  # pid = spawn(MyProcess, :awaiting_for_receive_messages, [])
  # Process.alive?(pid)
  # send(pid, "Hi")
end
