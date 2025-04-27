from collections import Counter

def compare_wordcounts(counts1, counts2):
    with open(counts1, 'r') as f1, open(counts2, 'r') as f2:
        words1 = " ".join(f1.readlines()).split()
        words2 = " ".join(f2.readlines()).split()
        count1 = Counter(words1)
        count2 = Counter(words2)
        return count1 == count2

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python compare_wordcounts.py <file1> <file2>")
        sys.exit(1)
        
    file1 = sys.argv[1]
    file2 = sys.argv[2]
    
    print("COUNTS MATCH? ", compare_wordcounts(file1, file2))