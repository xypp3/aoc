def parse(raw):
    return list(map(lambda x: list(map(int, x.split("-"))), raw.split(",")))


def part_1(seq):
    tot = 0
    for s in seq:
        for n in range(s[0], s[1] + 1):
            st = str(n)
            if st[len(st) // 2 :] == st[: len(st) // 2]:
                tot += n
    return tot


def part_2(seq):
    tot = 0
    for s in seq:
        for n in range(s[0], s[1] + 1):
            st = str(n)
            l = len(st)
            for i in range(l // 2):
                rep = l / (i + 1)
                if l % rep != 0:
                    continue
                if st == (st[: i + 1] * int(rep)):
                    # print(st[: i + 1], n)
                    tot += n
                    break
    return tot


if __name__ == "__main__":
    test = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
    tin = parse(test)
    with open("./puzzle_input/day02.txt", "r") as f:
        puz_in = parse(f.read())

    print(tin)
    print(part_1(tin))
    print(part_1(puz_in))
    print(part_2(tin))
    print(part_2(puz_in))
