def parse(txt):
    database = txt.strip().split("\n\n")
    assert len(database) == 2

    fresh = list(map(lambda l: list(map(int, l.split("-"))), database[0].split("\n")))
    ingred = list(map(int, database[1].split("\n")))

    # merge ranges
    fresh.sort(key=lambda f: f[0])
    i = 0
    while i < len(fresh) - 1:
        if fresh[i][1] < fresh[i + 1][0]:
            i += 1
            continue
        fresh[i][1] = max(fresh[i + 1][1], fresh[i][1])
        del fresh[i + 1]

    return fresh, ingred


def part_1(input):
    fresh, ingred = input
    total = 0

    def search_bin(target):
        l, r = 0, len(fresh) - 1
        while l <= r:
            m = (l + r) // 2
            s, e = fresh[m][0], fresh[m][1]

            if s <= target <= e:
                return True
            elif target < s:
                r = m - 1
            else:
                l = m + 1
        return False

    for i in ingred:
        if search_bin(i):
            total += 1

    return total


def part_2(input):
    fresh, _ = input
    total = 0

    for f in fresh:
        total += f[1] - f[0] + 1

    return total


if __name__ == "__main__":
    test = """3-5
10-14
16-20
12-18

1
5
8
11
17
32"""
    tin = parse(test)
    with open("./puzzle_input/day05.txt", "r") as f:
        puz = parse(f.read())
    print(tin)
    t1 = part_1(tin)
    print(t1)
    p1 = part_1(puz)
    print(p1)
    p2 = part_2(puz)
    print(p2)
