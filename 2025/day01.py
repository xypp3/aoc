test = """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""


def sign(num: int) -> int:
    if num == 0:
        return 0
    elif num < 0:
        return -1
    else:
        return 1


def part_1(input):
    dial = 50
    counter = 0
    for n in input:
        dial = (dial + 100 + n) % 100
        if dial == 0:
            counter += 1
    return counter


def part_2(input):
    dial = 50
    counter = 0
    for n in input:
        """
        1. |n| < 100
            - dial != 0 and dial + n <= 0
            - dial != 0 and dial + n >= 100
        2. |n| > 100
            - counter += abs(n // 100)
        """
        if abs(n) >= 100:
            spin = abs(n) // 100
            counter += spin
            n = (-1 if n < 0 else 1) * (abs(n) - spin * 100)
        if dial == 0:
            pass
        elif dial + n <= 0:
            counter += 1
        elif dial + n >= 100:
            counter += 1
        dial = (dial + n) % 100
    return counter


def parse(input):
    return list(
        map(lambda x: -1 * int(x[1:]) if x[0] == "L" else int(x[1:]), input.split())
    )


if __name__ == "__main__":
    tin = parse(test)
    c = part_1(tin)
    print(c)

    with open("puzzle_input/day_1.txt", "r") as f:
        fin = parse(f.read())

    c = part_1(fin)
    print(c)

    c = part_2(tin)
    print(c)

    with open("puzzle_input/day_1.txt", "r") as f:
        fin = parse(f.read())
    c = part_2(fin)
    print(c)
