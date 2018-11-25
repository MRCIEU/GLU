
# input dir, userID(not required) and output dir
dirx=$1
dirO=$2
filename=$3

if [ $filename ]
then
	echo "Processing file $filename in directory $dirx"

	sh convertFileFormat.sh $dirx $dirO $filename
elif [ $dirx ]
then
	echo "Processing all files in directory $dirx"

	# get all files in directory and run convert script on each	
	for f in ${dirx}*.*
	do		
		fname=$(basename "$f")
		
		echo "Processing file $fname"
		sh convertFileFormat.sh $dirx $dirO $fname
	done
fi

