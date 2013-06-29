grep '<http://schema.org/about>' $1 |grep '.*\.wikipedia\.org\/wiki' | awk '{
print $3,"<http://www.w3.org/2002/07/owl#sameAs>",$1
}' |sed s/'.wikipedia.org\/wiki'/'.dbpedia.org\/resource'/  > languagelinks.nt


