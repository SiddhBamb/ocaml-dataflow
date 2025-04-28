import random, string, csv, numpy as np, pandas as pd
from pathlib import Path

# Parameters
NUM_USERS = 4000
USERNAME_LEN = 6
NUM_ITEMS = 1000
RATINGS_PER_USER = 15              # 1000 * 10 = 10000
K = 8                               # latent factor dimension
NOISE_STD = 0.5
RNG = np.random.default_rng(seed=42)

# ---------- 1. Create users and items ----------
alphabet = string.ascii_letters + string.digits
users = [''.join(RNG.choice(list(alphabet), size=USERNAME_LEN)) for _ in range(NUM_USERS)]
items = np.arange(1, NUM_ITEMS + 1)

# ---------- 2. Sample latent factors ----------
user_factors = RNG.normal(size=(NUM_USERS, K))
item_factors = RNG.normal(size=(NUM_ITEMS, K))

# ---------- 3. Generate ratings ----------
records = []
for u_idx, user in enumerate(users):
    # pick 10 distinct items for this user
    item_indices = RNG.choice(NUM_ITEMS, size=RATINGS_PER_USER, replace=False)
    # vectorised dot‑product
    dots = item_factors[item_indices] @ user_factors[u_idx]
    dots += RNG.normal(scale=NOISE_STD, size=RATINGS_PER_USER)   # add noise
    ratings = np.clip(np.rint(dots + 3).astype(int), 1, 5)       # shift to 1–5 stars
    records.extend([(user, int(items[i]), int(r)) for i, r in zip(item_indices, ratings)])

# Confirm size
assert len(records) == NUM_USERS * RATINGS_PER_USER == 60000

# ---------- 4. Write to CSV ----------
out_path = Path('data/ratings_upper_medium.txt')
with out_path.open('w') as f:
    # Write header manually
    f.write(f"{NUM_USERS} {NUM_ITEMS} {NUM_USERS * RATINGS_PER_USER}\n")
    # Write records manually
    for record in records:
      f.write(f"{record[0]} {record[1]} {record[2]}")
      # write new line if not last record
      if record != records[-1]:
        f.write("\n")

# Show sample
sample_df = pd.DataFrame(records[:10], columns=['user', 'item', 'rating'])
# import ace_tools as tools; tools.display_dataframe_to_user("Sample of generated ratings", sample_df)

print(f"Generated {len(records):,} ratings and wrote to {out_path}")
