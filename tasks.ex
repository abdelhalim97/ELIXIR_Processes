#  agents, generic servers, and supervisors all work with multiple messages or manage state
# Tasks when we only need to execute some task and that is it

# Task.start_link/1 function that receives an anonymous(RunTasks.execute_task() in our case) function and executes it inside a new process
#  that will be part of a supervision tree.
defmodule TasksSupervisor do
  def start(_type, _args) do
    children = [
      # Task.Supervisor is a built-in one for one supervisor that starts temporary tasks as part of our supervision tree
      # Task.Supervisor has to start first
      {Task.Supervisor, name: RunTasks},
      Supervisor.child_spec({Task, fn -> RunTasks.execute_task() end}, restart: :temporary)
    ]

    IO.inspect("TasksSupervisor")

    opts = [strategy: :one_for_one, name: :supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule RunTasks do
  def execute_task() do
    IO.inspect("execute_task invoked ")
    IO.inspect(self())
    # Task.Supervisor.start_child(RunTasks, fn -> IO.inspect("Task executed PID: #{self()}") end)
  end
end

# Tasks, by default have the :restart value set to :temporary, which means they are not restarted
# if we want to restart them we have to set to restart: :permanent

# {:ok,sup}=TasksSupervisor.start(0,0) we lunch the app
