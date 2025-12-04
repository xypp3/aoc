def part_1(banks):
    total = 0
    for b in banks:
        if not b:
            continue
        # find largest leftmost digit
        tens = max(b[: len(b) - 1])
        i_tens = b.index(tens)
        ones = max(b[i_tens + 1 :])
        total += tens * 10 + ones

    return total


def part_2(banks):
    total = 0
    for b in banks:
        if not b:
            continue
        n = 0
        left_i = 0
        for i in range(11, -1, -1):
            s = b[left_i : len(b) - i]
            # need to find max and index of slice, not of original b (bank)
            left = max(s)
            left_i += s.index(left) + 1
            n += left * (10 ** (i))

        total += n

    return total


def parse(txt):
    return list(map(lambda l: list(map(int, l)), txt.split("\n")))


if __name__ == "__main__":
    test = """987654321111111
811111111111119
234234234234278
818181911112111"""
    with open("./puzzle_input/day03.txt", "r") as f:
        puz = parse(f.read())

    tin = parse(test)
    t1 = part_1(tin)
    print(t1)
    p1 = part_1(puz)
    print(p1)
    t2 = part_2(tin)
    print(t2)
    p2 = part_2(puz)
    print(p2)
