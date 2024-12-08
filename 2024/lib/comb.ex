defmodule Comb do
  def pairs([_]), do: []

  def pairs([head | tail]) do
    Enum.map(tail, &{head, &1}) ++ pairs(tail)
  end
end
