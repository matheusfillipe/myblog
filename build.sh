#!/usr/bin/env bash

minify=

get_org_attr(){
  file="$1"
  attr="$2"
  output=$(grep "#+$attr: " < "$file")
  output=$(echo "$output" | sed -e "s/^#+$attr: //")
  echo "$output"
}

# Fill LIST BEGIN/END for series
gen_index() {
  cd "$(dirname "$1")" || exit
  readarray -d '' entries < <(printf '%s\0' *.org | sort -zV)
  list=()
  for entry in "${entries[@]}"; do
    if [[ "$entry" == "index.org" ]]; then
      continue
    fi
    part_title=$(get_org_attr "$entry" "TITLE")
    description=$(get_org_attr "$entry" "DESCRIPTION")
    escaped=$(printf '%s\n' "- [[file:$entry][$part_title]]       $description" | sed -e 's/[]\/$*.^[]/\\&/g')
    list+="$escaped\n\n"
  done
  sed -e '1h;2,$H;$!d;g' -i -e "s/\(# LIST BEGIN\s*\n\).*\(# LIST END\s*\n\)/\1$list\2/ig" index.org
  cd ..
}

# Fill LIST BEGIN/END for ALL
gen_indexes() {
  cd series/ || exit
  series_list=""
  for index in */index.org
  do
    title=$(get_org_attr "$index" "TITLE")
    description=$(get_org_attr "$index" "DESCRIPTION")
    gen_index "$index"
    if [ -z "$title" ]; then
      echo "No title found for index: $index"
      continue
    else
      escaped=$(printf '%s\n' "- [[file:$index][$title]]" | sed -e 's/[]\/$*.^[]/\\&/g')
      series_list+="$escaped\n\n"
    fi
  done
  sed -e '1h;2,$H;$!d;g' -i -e "s/\(# LIST BEGIN\s*\n\).*\(# LIST END\s*\n\)/\1$series_list\2/ig" index.org
  cd ..
}

# Get last 6 published posts and display on /index.html list
gen_featured() {
  count=6
  files=$(
    find series/ -name "*.org" ! -name "index.*" | while read f
    do
      echo "$(git log --format="%at" --reverse "$f" | head -n1) $f"
    done | sort -n | tail -n$count | sed 's/^[[:digit:]]\+ //'
  )
  IFS=$'\n' read -r -d '' -a orglist <<< "$files"

  featured_list=""
  for entry in ${orglist[@]}
  do
    description=$(get_org_attr "$entry" "DESCRIPTION")
    part_title=$(get_org_attr "$entry" "TITLE")
    escaped=$(printf '%s\n' "- [[file:$entry][$part_title]]" | sed -e 's/[]\/$*.^[]/\\&/g')
    # escaped=$(printf '%s\n' "- [[file:$entry][$part_title]]       $description" | sed -e 's/[]\/$*.^[]/\\&/g')
    featured_list+="$escaped\n\n"
  done
  sed -e '1h;2,$H;$!d;g' -i -e "s/\(# LIST FEATURED BEGIN\s*\n\).*\(# LIST FEATURED END\s*\n\)/\1$featured_list\2/ig" index.org
}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "$SCRIPT_DIR"

rm -rf html

gen_featured
gen_indexes

mkdir -p html
cp style.css html/
cp code.css html/
cp script.js html/
cp comments.js html/
cp utils.js html/
cp background.js html/
cp favicon.ico html/
cp -r assets html/
emacs -Q --script build-site.el

# Generate a proper sitemap
echo "Generating json sitemap"
cd html/
tree -J -P "*.html" -I "footer.html" -I "header.html" -I "assets" -I "sitemap.html" > sitemap.json
cd ..

if [ $minify ]
then
  echo "Minifying..."
  shopt -s globstar nullglob dotglob
  for f in html/**/*.{css,html,js,json}
  do
    ./minify "$f" -o "$f"
  done
fi

echo "All done!"
