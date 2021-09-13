import Data.Hash.MD5

input = "yzbqklnj"

findN hashPrefixLength prefix number =
  if valid then number else findN hashPrefixLength prefix (number + 1)
  where
    valid = all (== '0') . take hashPrefixLength $ md5s (Str (prefix ++ show number))

part1 prefix = findN 5 prefix 0

part2 prefix = findN 6 prefix 0
