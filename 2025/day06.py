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
    rows_str = txt.split("\n")
    rows_str = rows_str[: len(rows_str) - 1]
    rows_str, ops_str = rows_str[: len(rows_str) - 1], rows_str[-1]

    ops_list = []
    num_lens = []
    tmp = " "
    for c in ops_str:
        if c == "+" or c == "*":
            num_lens.append(len(tmp) - 1)
            ops_list.append(tmp[0])
            tmp = c
            continue
        tmp += c
    ops_list.append(tmp[0])
    num_lens.append(len(tmp))
    del ops_list[0]
    del num_lens[0]

    rows = []
    for r in rows_str:
        row = []
        i = 0
        for n in num_lens:
            row.append(r[i : i + n])
            i += n + 1  # NOTE: skip the space char with +1
        rows.append(row)

    cols = []
    for c in range(len(rows[0])):
        col = []
        for i in range(num_lens[c]):
            s = ""
            for r in range(len(rows)):
                s += rows[r][c][i]
            # NOTE: test is " " * 3 and puzzle input is " " * 4
            col.append(0 if " " * 3 == s or " " * 4 == s else int(s))
        cols.append(col)

    return cols, ops_list


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
*   +   *   +  
"""
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
    puz2 = parse_2(file)
    p2 = part_2(puz2)
    print(p2)
