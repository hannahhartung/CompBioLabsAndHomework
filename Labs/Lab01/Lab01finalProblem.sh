cut -f 2-4 -d , PredPreyData.csv > SummaryData.txt
cut -f 2-4 -d , PredPreyData.csv | tail
grep \" SummaryData.txt >  Question5.txt
tail SummaryData.txt >>  Question5.txt