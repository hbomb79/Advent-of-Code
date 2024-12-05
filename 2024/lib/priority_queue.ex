defmodule PriorityQueue do
  alias __MODULE__, as: PQ

  defstruct tree: :gb_trees.empty(), dict: Map.new()
  @type t() :: %PQ{tree: :gb_trees.tree(), dict: map()}
  @type priority :: any()
  @type key :: any()
  @type value :: non_neg_integer()
  @type token :: {priority(), reference()}

  @spec new() :: t()
  def new(), do: %PQ{}

  @spec empty?(t()) :: boolean()
  def empty?(%PQ{tree: {0, _}}), do: true
  def empty?(%PQ{}), do: false

  @spec add(t(), priority(), key(), value()) :: t()
  def add(%PQ{tree: tree, dict: dict}, pri, key, val) do
    false = Map.has_key?(dict, key)
    token = {pri, make_ref()}

    %PQ{
      tree: :gb_trees.insert(token, key, tree),
      dict: Map.put(dict, key, {token, val})
    }
  end

  @spec pop(t()) :: {key(), value(), t()}
  def pop(%PQ{tree: tree} = pq) do
    {{pri, _}, key, ntree} = :gb_trees.take_smallest(tree)
    {pri, key, %{pq | tree: ntree}}
  end

  @spec mapping(t(), key()) :: {token(), value()} | {nil, nil}
  def mapping(%PQ{dict: dict}, key) do
    Map.get(dict, key) || {nil, nil}
  end

  @spec delete(t(), token(), key()) :: t()
  def delete(%PQ{tree: tree, dict: dict}, token, key) do
    %PQ{tree: :gb_trees.delete(token, tree), dict: Map.delete(dict, key)}
  end

  @spec get_by_key(t(), key()) :: value()
  def get_by_key(%PQ{dict: dict}, key) do
    {_, val} = Map.get(dict, key)
    val
  end

  defmacro empty do
    quote do: %PQ{tree: {0, _}}
  end
end

defimpl Collectable, for: PriorityQueue do
  def into(original) do
    {original,
     fn
       h, {:cont, {p, k, v}} -> h |> PriorityQueue.add(p, k, v)
       h, :done -> h
       _, :halt -> :ok
     end}
  end
end
