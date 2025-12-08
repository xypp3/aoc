def part_1(grid):
    count = 0
    for i in range(1, len(grid)):
        for j in range(len(grid[0])):
            if grid[i - 1][j] == ".":
                continue
            elif grid[i - 1][j] == "^":
                continue

            if grid[i][j] == ".":
                grid[i][j] = "|"
            elif grid[i][j] == "^":
                count += 1
                grid[i][max(0, j - 1)] = "|"
                grid[i][min(len(grid[0]) - 1, j + 1)] = "|"

    return count


def part_2(grid):
    cache = {}

    def search(timelines, r, c):
        if (r, c) in cache:
            return cache[(r, c)]
        while grid[r][c] != "^":
            # NOTE: BASE CASE
            if r == len(grid) - 1:
                return 1
            r += 1

        left, right = 0, 0
        if 0 <= c - 1:
            left = search(timelines, r + 1, c - 1)
            cache[(r + 1, c - 1)] = left
        if c + 1 < len(grid[0]):
            right = search(timelines, r + 1, c + 1)
            cache[(r + 1, c + 1)] = right

        return left + right

    return search(0, 0, grid[0].index("|"))


def print_grid(grid):
    for r in grid:
        print(r)


def parse(txt):
    o = list(map(lambda l: list(l), txt.strip().split("\n")))
    # NOTE: assumes only 1 'S' at top
    for i in range(len(o[0])):
        if o[0][i] == "S":
            o[0][i] = "|"
            break
    return o


if __name__ == "__main__":
    test = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""
    mini = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
"""
    with open("./puzzle_input/day07.txt", "r") as f:
        fin = parse(f.read())

    tin = parse(test)
    t1 = part_1(tin)
    print(t1)
    p1 = part_1(fin)
    print(p1)
    m2 = part_2(parse(mini))
    print(m2)
    t2 = part_2(tin)
    print(t2)
    p2 = part_2(fin)
    print(p2)
