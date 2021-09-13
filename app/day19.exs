defmodule Day19 do
  def test() do
    """
    e => H
    e => O
    H => HO
    H => OH
    O => HH
    """
  end

  def solve() do
    i = input()
    r = parse_replacements(i)
    s = parse_sequence(i)

    solve(0, s, r)
  end

  def solve(i, "e", _r), do: i

  def solve(i, s, r) do
    IO.inspect(s, label: i)

    next =
      Enum.find_value(r, fn {m, r} ->
        if String.contains?(s, m) do
          String.replace(s, m, r, global: false)
        end
      end)

    solve(i + 1, next, r)
  end

  def input do
    File.read!("./app/input/day19.txt")
  end

  def parse_replacements(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.filter(&String.contains?(&1, "=>"))
    |> Enum.map(fn line ->
      line
      |> String.split(" => ")
      |> Enum.reverse()
      |> List.to_tuple()
    end)
    |> Enum.sort_by(fn {x, _} -> String.length(x) end, :desc)
  end

  def parse_sequence(input) do
    input
    |> String.split("\n", trim: true)
    |> List.last()
  end
end

# Day19.solve(0, "HOHOHO", Day19.parse_replacements(Day19.test()))
Day19.solve()
|> IO.inspect()
