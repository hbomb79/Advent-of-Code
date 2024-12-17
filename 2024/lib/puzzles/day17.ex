defmodule Puzzles.Day17 do
  def part1(input) do
    {program, registers} = parse(input)
    run(0, program, registers, []) |> Enum.join(",")
  end

  defp run(instp, instructions, _, out) when instp == length(instructions), do: Enum.reverse(out)

  defp run(instp, instructions, %{a: a, b: b, c: c} = registers, out) do
    opcode = Enum.at(instructions, instp)
    operand = Enum.at(instructions, instp + 1)

    case opcode do
      0 ->
        # adv: a-register division
        res = div(a, 2 ** combo_operand(operand, registers))
        run(instp + 2, instructions, %{registers | :a => res}, out)

      1 ->
        # bxl: b-register xor literal
        run(instp + 2, instructions, %{registers | :b => Bitwise.bxor(b, operand)}, out)

      2 ->
        # bst: b-register bitshift three (% 8)
        res = combo_operand(operand, registers) |> rem(8)
        run(instp + 2, instructions, %{registers | :b => res}, out)

      3 ->
        # jnz: jump if not zero
        case a do
          0 -> run(instp + 2, instructions, registers, out)
          _ -> run(operand, instructions, registers, out)
        end

      4 ->
        # bxc: b-register XOR c-register
        run(instp + 2, instructions, %{registers | :b => Bitwise.bxor(b, c)}, out)

      5 ->
        # out
        res = combo_operand(operand, registers) |> rem(8)
        run(instp + 2, instructions, registers, [res | out])

      6 ->
        # bdv: b-register division
        res = div(a, 2 ** combo_operand(operand, registers))
        run(instp + 2, instructions, %{registers | :b => res}, out)

      7 ->
        # cdv: c-register division
        res = div(a, 2 ** combo_operand(operand, registers))
        run(instp + 2, instructions, %{registers | :c => res}, out)
    end
  end

  defp combo_operand(operand, _) when operand in 0..3, do: operand
  defp combo_operand(4, %{:a => v}), do: v
  defp combo_operand(5, %{:b => v}), do: v
  defp combo_operand(6, %{:c => v}), do: v

  def part2(input) do
    {program, registers} = parse(input)

    # Search from the end of the program because we know (by reverse engineering the program) that the
    # value of A is divided by 8 each iteration. Therefore, we can multiply the value of A we need to get only
    # the last digit of the output to narrow our search space.
    #
    # First, find the value of A that gives us a single output [?] which matches the last digit of our program
    # Then, multiply the found value of A by 8, and search again; but this time for a value of A that gives us TWO outputs [?, ?] which match the last two values of the program
    # Then, multiply the found value of A by 8 again, and search again for the last 3 digits
    # ... and so on ...
    find(program, registers, program)
  end

  defp find(program, registers, to_find) do
    a_val =
      case to_find do
        [_] -> 0
        [_ | rest] -> 8 * find(program, registers, rest)
      end

    find_recursive(a_val, program, registers, to_find)
  end

  defp find_recursive(a_val, program, registers, to_find) do
    case run(0, program, %{registers | :a => a_val}, []) do
      ^to_find -> a_val
      _ -> find_recursive(a_val + 1, program, registers, to_find)
    end
  end

  defp parse(input) do
    [registers, program] = String.split(input, "\n\n", trim: true)

    registers =
      String.split(registers, "\n", trim: true)
      |> Enum.zip([:a, :b, :c])
      |> Enum.map(fn {reg, name} ->
        [_, val] = String.split(reg, ": ", trim: true)
        {name, String.to_integer(val)}
      end)
      |> Map.new()

    program =
      String.split(program, ": ", trim: true)
      |> List.last()
      |> String.trim_trailing()
      |> String.split(",", trim: true)
      |> Enum.map(&String.to_integer/1)

    {program, registers}
  end
end
