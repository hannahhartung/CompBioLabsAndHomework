cut -f 2-4 -d , PredPreyData.csv > SummaryData.txt
grep \" SummaryData.txt >  Question5.txt
tail SummaryData.txt >>  Question5.txt