#!/bin/sh


#mkdir -p llinkfiles
filename=$1

gawk < $filename '{
match ($3 , /<http:\/\/(.*).dbpedia.org\/resource\/.*>/ ,m)
{
Q=$1
lng=m[1]

gsub("-","_",m[1])
system("touch ./llinkfiles/interlanguage_links_same_as_" m[1] ".ttl")
print $1,$2,$3,"." >> "./llinkfiles/interlanguage_links_same_as_" m[1] ".ttl"

}
}'




