import random

n = 10000
d = 5

# Generate the header line
header = f"{n} {d}"

# Generate the data lines
data_lines = []
for _ in range(n):
  point = [str(random.random()) for _ in range(d)]
  data_lines.append(" ".join(point))

# Combine header and data
output_content = header + "\n" + "\n".join(data_lines)

# Print the content to kmeansdata_large.txt
with open("data/kmeansdata_medium.txt", "w") as f:
  f.write(output_content)