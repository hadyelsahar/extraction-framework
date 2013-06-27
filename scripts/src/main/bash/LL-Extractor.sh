grep '<http://schema.org/about>' $1 |grep '.*\.wikipedia\.org\/wiki' | awk '{
print $3,$2,$1
}' |sed s/'.wikipedia.org\/wiki'/'.dbpedia.org\/resource'/  > languagelinks.nt