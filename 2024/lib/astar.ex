defmodule AStar do
  @moduledoc """
  Shortest-path implementation (A*) for a generic data source (i.e. functions are used for neighbours/costs)
  """
  require PriorityQueue

  @type vertex() :: any()

  @typedoc """
  A function which must return the valid neighbours of a given vertex. The results of
  this function do not need to care about any visited vertices - these are filtered out by
  the algorithm.
  """
  @type neighbour_fn() :: (vertex() -> [vertex()])

  @typedoc """
  A function which reports the cost for a vertex. This function
  is used for both the G and H costs in the algorithm, but the type is shared.
  """
  @type cost_fn() :: (vertex() -> non_neg_integer())

  @typedoc """
  A function which tests if a given vertex is the goal point; any 'truthy' value will
  consider the algorithm complete.
  """
  @type goal_fn() :: (vertex() -> as_boolean(any()))

  @type state() :: {neighbour_fn(), g :: cost_fn(), h :: cost_fn()}

  @spec shortest_path(state(), vertex(), goal_fn()) :: list()
  def shortest_path({_, _, h} = env, start, goal) do
    queue = PriorityQueue.new() |> PriorityQueue.add(h.(start), start, 0)
    find(env, goal, queue, MapSet.new(), Map.new())
  end

  defp find(_, _, PriorityQueue.empty(), _, _), do: []

  defp find({neighbours, g, h} = state, goal, queue, closedset, parents) do
    {_, vertex, queue} = PriorityQueue.pop(queue)

    if goal.(vertex) do
      # Done, build a path from this vertex from the parents
      construct_path(parents, vertex)
    else
      # Prevent re-visiting of this vertex
      closedset = MapSet.put(closedset, vertex)

      # Iterate neighbours, adding any non-visited nodes to the queue using 'update'
      {queue, parents} =
        Enum.reduce(
          neighbours.(vertex),
          {queue, parents},
          fn candidate, {queue, parents} = acc ->
            if MapSet.member?(closedset, candidate) do
              # Already visited this candidate, ignore
              acc
            else
              # Get gscore for this candidate, by combining with the gscore for the current vertex
              gscore = PriorityQueue.get_by_key(queue, vertex) + g.(candidate)

              # Get any existing gscore for this candidate (and it's unique token, for tree operations)
              case PriorityQueue.mapping(queue, candidate) do
                {nil, nil} ->
                  # No existing cost for 'candidate', add one
                  update(h, vertex, candidate, gscore, queue, parents)

                {token, gy} when gscore < gy ->
                  # Cost we've found (gscore) is better than the existing one (gy), replace
                  queue = queue |> PriorityQueue.delete(token, candidate)
                  update(h, vertex, candidate, gscore, queue, parents)

                _ ->
                  # Cost we've already found for 'candidate' is better, ignore
                  acc
              end
            end
          end
        )

      find(state, goal, queue, closedset, parents)
    end
  end

  defp update(h, parent, candidate, candidate_gscore, queue, parents) do
    # Store the parent for this candidate vertex so we can construct a path to it later
    nparents = Map.put(parents, candidate, parent)

    # Calculate 'fscore' by combining hscore and gscore for candidate vertex.
    # The fscore is used to order to priority queue, and the g score
    # is retained in the value of each element so we can refer to it in the future.
    fscore = h.(candidate) + candidate_gscore
    nqueue = queue |> PriorityQueue.add(fscore, candidate, candidate_gscore)
    {nqueue, nparents}
  end

  @spec construct_path(Dict.t(), vertex) :: [vertex]
  defp construct_path(parents, vertex), do: construct_path(parents, vertex, [])

  defp construct_path(parents, vertex, acc) do
    case Map.get(parents, vertex) do
      nil -> acc
      parent -> construct_path(parents, parent, [vertex | acc])
    end
  end
end
