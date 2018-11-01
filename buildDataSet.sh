rm *labels*
rm *param*

cut -c 1,2 Treino_100.txt > aux_100labels.txt
cut -d"," -f1 --complement Treino_100.txt > Treino_100param.txt
tr -d ',' <  aux_100labels.txt > Treino_100labels.txt 
rm aux_100labels.txt

cut -c 1,2 Treino_200.txt > aux_200labels.txt
cut -d"," -f1 --complement Treino_200.txt > Treino_200param.txt
tr -d ',' <  aux_200labels.txt > Treino_200labels.txt 
rm aux_200labels.txt

cut -c 1,2 Treino_300.txt > aux_300labels.txt
cut -d"," -f1 --complement Treino_300.txt > Treino_300param.txt
tr -d ',' <  aux_300labels.txt > Treino_300labels.txt 
rm aux_300labels.txt

cut -c 1,2 Treino_400.txt > aux_400labels.txt
cut -d"," -f1 --complement Treino_400.txt > Treino_400param.txt
tr -d ',' <  aux_400labels.txt > Treino_400labels.txt 
rm aux_400labels.txt

cut -c 1,2 Treino_500.txt > aux_500labels.txt
cut -d"," -f1 --complement Treino_500.txt > Treino_500param.txt
tr -d ',' <  aux_500labels.txt > Treino_500labels.txt 
rm aux_500labels.txt

cut -c 1,2 Treino_600.txt > aux_600labels.txt
cut -d"," -f1 --complement Treino_600.txt > Treino_600param.txt
tr -d ',' <  aux_600labels.txt > Treino_600labels.txt 
rm aux_600labels.txt

cut -c 1,2 Treino_700.txt > aux_700labels.txt
cut -d"," -f1 --complement Treino_700.txt > Treino_700param.txt
tr -d ',' <  aux_700labels.txt > Treino_700labels.txt 
rm aux_700labels.txt

cut -c 1,2 Treino_800.txt > aux_800labels.txt
cut -d"," -f1 --complement Treino_800.txt > Treino_800param.txt
tr -d ',' <  aux_800labels.txt > Treino_800labels.txt 
rm aux_800labels.txt

cut -c 1,2 Treino_900.txt > aux_900labels.txt
cut -d"," -f1 --complement Treino_900.txt > Treino_900param.txt
tr -d ',' <  aux_900labels.txt > Treino_900labels.txt
rm aux_900labels.txt

cut -c 1,2 Treino_1000.txt > aux_1000labels.txt
cut -d"," -f1 --complement Treino_1000.txt > Treino_1000param.txt
tr -d ',' <  aux_1000labels.txt > Treino_1000labels.txt
rm aux_1000labels.txt

cut -c 1,2 Teste_1000.txt > aux-Teste_1000labels.txt
cut -d"," -f1 --complement Teste_1000.txt > Teste_1000param.txt
tr -d ',' <  aux-Teste_1000labels.txt > Teste_1000labels.txt
rm aux-Teste_1000labels.txt

cut -c 1,2 Teste_1000_10db.txt > aux-Teste_1000_10dblabels.txt
cut -d"," -f1 --complement Teste_1000_10db.txt > Teste_1000_10dbparam.txt
tr -d ',' <  aux-Teste_1000_10dblabels.txt > Teste_1000_10dblabels.txt
rm aux-Teste_1000_10dblabels.txt

cut -c 1,2 Teste_1000_20db.txt > aux-Teste_1000_20dblabels.txt
cut -d"," -f1 --complement Teste_1000_20db.txt > Teste_1000_20dbparam.txt
tr -d ',' <  aux-Teste_1000_20dblabels.txt > Teste_1000_20dblabels.txt
rm aux-Teste_1000_20dblabels.txt

cut -c 1,2 Teste_1000_30db.txt > aux-Teste_1000_30dblabels.txt
cut -d"," -f1 --complement Teste_1000_30db.txt > Teste_1000_30dbparam.txt
tr -d ',' <  aux-Teste_1000_30dblabels.txt > Teste_1000_30dblabels.txt
rm aux-Teste_1000_30dblabels.txt

cut -c 1,2 Teste_1000_40db.txt > aux-Teste_1000_40dblabels.txt
cut -d"," -f1 --complement Teste_1000_40db.txt > Teste_1000_40dbparam.txt
tr -d ',' <  aux-Teste_1000_40dblabels.txt > Teste_1000_40dblabels.txt
rm aux-Teste_1000_40dblabels.txt

cut -c 1,2 Teste_1000_50db.txt > aux-Teste_1000_50dblabels.txt
cut -d"," -f1 --complement Teste_1000_50db.txt > Teste_1000_50dbparam.txt
tr -d ',' <  aux-Teste_1000_50dblabels.txt > Teste_1000_50dblabels.txt
rm aux-Teste_1000_50dblabels.txt


                        



