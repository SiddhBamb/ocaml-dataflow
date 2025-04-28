from collections import Counter

def compare_wordcounts(counts1, counts2):
    with open(counts1, 'r') as f1, open(counts2, 'r') as f2:
        words1 = f1.readlines()
        words2 = f2.readlines()
        a1 = [w.strip().split(":") for w in words1]
        a2 = [w.strip().split(":") for w in words2]
        c1 = {x[0]: x[1] for x in a2}
        c2 = {x[0]: x[1] for x in a1}
        return c1 == c2

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python compare_wordcounts.py <file1> <file2>")
        sys.exit(1)
        
    file1 = sys.argv[1]
    file2 = sys.argv[2]
    
    print("COUNTS MATCH? ", compare_wordcounts(file1, file2))