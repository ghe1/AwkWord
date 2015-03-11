def main():
    nl = open('newlist.txt', 'r')
    out = open('Wordlist.elm', 'w')
    out.write("module Wordlist (wl) where\n\n")
    out.write("import List (..)\n")
    out.write("import Set (..)\n\n")
    out.write("wl : Set String\n")
    out.write("wl = fromList wordlist\n\n")
    out.write("wordlist : List String\n")
    out.write("wordlist = \n")
    out.write("  [ ")
    for line in nl:
        l = line.rstrip()
        out.write("\"" + l + "\"" + "\n  , ")
    out.write("]")

if __name__ == "__main__":
    main()
