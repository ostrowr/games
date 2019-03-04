# poker

To generate the hand-rankings cache:

1. `stack run print-mappings > sevenToBestFive.tsv`
2. `sort -n -o sevenToBestFive.sorted.tsv sevenToBestFive.tsv` (5 GB)
3. `zip sevenToBestFive.sorted.zip sevenToBestFive.sorted.tsv` (529 MB, and can be streamed into memory)
