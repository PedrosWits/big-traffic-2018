thresholds=( 450 600 900 1200 1800)

for i in "${thresholds[@]}"
do
    echo "***************RUNNING AS_TRIPS*******************"
    Rscript as_trips.R $i
done
