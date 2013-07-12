mkdir -p llinkfiles

gawk < $1 '{
match ($3 , /<http:\/\/(.*).dbpedia.org\/resource\/.*>/ ,m)
{
gsub("-","_",m[1])
system("touch ./llinkfiles/interlanguage_links_same_as_" m[1] ".ttl")
print $1,$2,$3,"." >> "./llinkfiles/interlanguage_links_same_as_" m[1] ".ttl"
}
}'
