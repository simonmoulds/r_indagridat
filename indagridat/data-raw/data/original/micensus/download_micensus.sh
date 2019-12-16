#!/bin/sh

# statewise urls:

count=1
while read line; do
    if [[ $line == https* ]]; then
	#url=$line
	file=$(awk 'NR == nr' nr=$((count+1)) micensus_statewise_distrsys_urls.txt)
	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
        echo $url > temp
	wget -O $file -i temp 
    fi
    count=$((count+1))
done < micensus_statewise_distrsys_urls.txt

# not used:

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_districtwise_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_districtwise_urls.txt

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_dugwell_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_dugwell_urls.txt

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_deeptubewell_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_deeptubewell_urls.txt

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_shallowtubewell_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_shallowtubewell_urls.txt

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_surfacelift_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_surfacelift_urls.txt

# count=1
# while read line; do
#     if [[ $line == https* ]]; then
# 	#url=$line
# 	file=$(awk 'NR == nr' nr=$((count+1)) micensus_surfaceflow_urls.txt)
# 	url=$(curl $line | grep window.location.href | sed 's/.*= //' | sed 's/"//g') 									     
#         echo $url > temp
# 	wget -O $file -i temp 
#     fi
#     count=$((count+1))
# done < micensus_surfaceflow_urls.txt

