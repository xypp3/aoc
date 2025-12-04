def print_grid(grid):
    for r in grid:
        print(r)


def parse(txt):
    return list(map(lambda l: list(l), txt.split("\n")))


def count_surrounding(grid, y, x):
    assert len(grid) > 0
    m = len(grid)
    n = len(grid[0])

    assert 0 <= y and 0 <= x and y < m and x < n

    count = 0

    for i in range(-1, 2):
        for j in range(-1, 2):
            if not (0 <= y + i and 0 <= x + j and y + i < m and x + j < n):
                continue
            if i == 0 and j == 0:
                continue
            if grid[y + i][x + j] == "@":
                count += 1

    return count


def part_1(grid):
    total = 0
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == "@" and count_surrounding(grid, i, j) < 4:
                total += 1
    return total, grid


def part_2(grid):
    total = 0
    changes = []
    while True:
        for i in range(len(grid)):
            for j in range(len(grid[0])):
                if grid[i][j] == "@" and count_surrounding(grid, i, j) < 4:
                    changes.append((i, j))
                    total += 1
        if not changes:
            break
        for i, j in changes:
            grid[i][j] = "x"
        changes = []

    return total


if __name__ == "__main__":
    test = """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."""

    tin = parse(test)
    with open("./puzzle_input/day04.txt", "r") as f:
        puz = parse(f.read())
        puz = puz[: len(puz) - 1]

    c, g = part_1(tin)
    print(c)
    print_grid(g)
    c, _ = part_1(puz)
    print(c)
    t2 = part_2(tin)
    print(t2)
    p2 = part_2(puz)
    print(p2)
