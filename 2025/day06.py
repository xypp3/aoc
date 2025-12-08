import re
import math


def parse(txt):
    rows = txt.strip().split("\n")
    return list(
        map(
            lambda r: list(map(int, re.split(r"\s+", r.strip()))), rows[: len(rows) - 1]
        )
    ), re.split(r"\s+", rows[-1])


def part_1(input):
    grid, op_list = input
    grand_total = 0
    for col in range(len(grid[0])):
        op = op_list[col]
        tmp = 0 if op == "+" else 1
        for row in range(len(grid)):
            tmp = tmp + grid[row][col] if op == "+" else tmp * grid[row][col]
        grand_total += tmp
    return grand_total


def parse_2(txt):
    rows = txt.strip().split("\n")
    rows, ops = rows[: len(rows) - 1], re.split(r"\s+", rows[-1])
    col_segments = list(
        map(lambda s: [s[i : i + 3] for i in range(0, len(s), 4)], rows)
    )
    nums = []
    for col in range(len(col_segments[0])):
        tmp = []
        for i in range(3):
            s = ""
            for row in range(3):
                s += col_segments[row][col][i]
            tmp.append(0 if s == "   " else int(s))
        nums.append(tmp)

    return nums, ops


def part_2(input):
    cols, ops = input
    grand_total = 0
    for i, c in enumerate(cols):
        grand_total += sum(c) if ops[i] == "+" else math.prod(c)
    return grand_total


if __name__ == "__main__":
    test = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +"""
    with open("./puzzle_input/day06.txt", "r") as f:
        file = f.read()

    tin = parse(test)
    print(tin)
    t1 = part_1(tin)
    print(t1)
    puz = parse(file)
    p1 = part_1(puz)
    print(p1)
    tin2 = parse_2(test)
    print(tin2)
    t2 = part_2(tin2)
    print(t2)
    p2 = part_2(parse_2(file))
    print(p2)
