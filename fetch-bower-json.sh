repository=purescript/purescript-newtype

curl -s "https://github.com/$repository/tags.atom" |
    perl -ne 'print "$1\n" if m#<title>(v?\d+\.\d+\.\d+)</title>#' |
    awk 'NR < 4' |
    while read -r version; do
        mkdir -p "$repository/$version"
        curl "https://raw.githubusercontent.com/$repository/$version/bower.json" > "$repository/$version/bower.json"
    done
