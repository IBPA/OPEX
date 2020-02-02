for f in *
do
#sed -i 's/, /, /g' $f
# sed -i 's/, */, /g' $f
#sed -i 's/ == / == /g' $f
#sed -i 's/ * == */ == /g' $f
#sed -i 's/ != / != /g' $f
# sed -i 's/ * != */ != /g' $f
#sed -i 's/"batch_size"/"batch_size"/g' $f
# sed -i 's/ <- / <- /g' $f
sed -i 's/adaptive/adaptive/g' $f
done
