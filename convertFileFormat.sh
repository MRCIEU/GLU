
# user id of file we want to process
dir=$1
dirO=$2
filename=$3

# fix file encoding
iconv -f UTF-16 -t ISO-8859-15 ${dir}$filename > ${dirO}$filename


# remove header lines
awk 'NR > 11 { print }' ${dirO}$filename > ${dirO}${filename}TMP && mv ${dirO}${filename}TMP ${dirO}$filename

# select columns we want to use

cat ${dirO}$filename > ${dirO}${filename}TMP && mv ${dirO}${filename}TMP ${dirO}$filename




sed -i 's/GSR/0/g' ${dirO}$filename
sed -i 's/Log sheet/1/g' ${dirO}$filename


