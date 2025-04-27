from collections import Counter

def wordcount(filename):
    with open(filename) as f:
        return Counter(" ".join(f.readlines()).split())

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python count_words.py <filename>")
        sys.exit(1)
    
    filename = sys.argv[1]
    counts = wordcount(filename)

    if not counts:
        print("No words found in the file.")
    
    for word, count in sorted(counts.items()):
        print(f"{word}: {count}")
    

    
    

