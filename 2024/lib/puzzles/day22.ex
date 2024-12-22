defmodule Puzzles.Day22 do
  def part1(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.to_integer(line) |> get_secrets() |> List.last()
    end)
    |> Enum.sum()
  end

  def part2(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.to_integer(line) |> gen_prices() |> gen_deltas() |> get_scores()
    end)
    |> Enum.reduce(%{}, fn scores, acc ->
      Enum.reduce(scores, acc, fn {k, v}, acc ->
        Map.update(acc, k, v, fn e -> v + e end)
      end)
    end)
    |> Map.values()
    |> Enum.max()
  end

  defp get_scores(pd) do
    Enum.chunk_every(pd, 4, 1)
    |> Enum.reduce(%{}, fn
      [{_, ac}, {_, bc}, {_, cc}, {dp, dc}], acc -> Map.update(acc, {ac, bc, cc, dc}, dp, & &1)
      _, acc -> acc
    end)
  end

  defp gen_prices(secret) do
    [secret | get_secrets(secret, 2000, [])] |> Enum.map(&rem(&1, 10))
  end

  defp gen_deltas([p | ps]) do
    Enum.reduce(ps, [{p, 0}], fn p, [{last_price, _last_delta} | _] = acc ->
      [{p, p - last_price} | acc]
    end)
    |> Enum.reverse()
  end

  defp get_secrets(secret, n \\ 2000, acc \\ [])
  defp get_secrets(_secret, 0, secrets), do: Enum.reverse(secrets)

  defp get_secrets(secret, n, acc) do
    next = next_secret(secret)
    get_secrets(next, n - 1, [next | acc])
  end

  defp next_secret(secret) do
    # Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
    nsecret = mix(secret, secret * 64) |> prune()

    # Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
    nsecret2 = (nsecret / 32) |> floor()
    nsecret2 = mix(nsecret, nsecret2) |> prune()

    # Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.
    nsecret3 = nsecret2 * 2048
    mix(nsecret2, nsecret3) |> prune()
  end

  defp mix(a, b), do: Bitwise.bxor(a, b)
  defp prune(secret), do: rem(secret, 16_777_216)
end
