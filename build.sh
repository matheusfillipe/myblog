#!/usr/bin/env bash

get_org_attr(){
  file="$1"
  attr="$2"
  output=$(grep "#+$attr: " < "$file")
  output=$(echo "$output" | sed -e "s/^#+$attr: //")
  echo "$output"
}

gen_index() {
  cd "$(dirname "$1")" || exit
  readarray -d '' entries < <(printf '%s\0' *.org | sort -zV)
  for entry in "${entries[@]}"; do
    if [[ "$entry" == "index.org" ]]; then
      continue
    fi
    title=$(get_org_attr "$entry" "TITLE")
    description=$(get_org_attr "$entry" "DESCRIPTION")
    escaped=$(printf '%s\n' "- [[file:$entry][$title]]       $description" | sed -e 's/[]\/$*.^[]/\\&/g')
    list+="$escaped\n\n"
  done
  sed -e '1h;2,$H;$!d;g' -i -e "s/\(# LIST BEGIN\s*\n\).*\(# LIST END\s*\n\)/\1$list\2/ig" index.org
  cd ..
}

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

gen_indexes


mkdir -p html
cp style.css html/
cp script.js html/
cp -r assets html/
emacs -Q --script build-site.el
