library(dplyr)
x<-Gli1_New
arrange(x,x$Position)


end_frame = filter(x, x$Position %in% target)
target =  c(  
   88,
   89,
   100,
   113,
   149,
   161,
   171,
   177,
   194,
   210,
   227,
   263,
   265,
   267,
   274,
   314,
   316,
   372,
   380,
   380,
   405,
   469,
   497,
   498,
   558,
   572,
   613,
   637,
   787,
   914,
   917,
   972,
   1030,
   1094
)

unique_targets_hit = unique(end_frame$Position)


vector = c() 
for (i in unique_targets_hit){
    range = c(i - 4, i-3,i-2,i-1,i,i+1,i+2,i+3,i+4)
    vector = c(vector,range)
}

unique_range_hits = unique(vector)

end_frame = filter(x, x$Position %in% vector)
end_frame = arrange(end_frame,end_frame$Position)

end_frame$target = 0
for (i in target){
  range = c(i - 4, i-3,i-2,i-1,i,i+1,i+2,i+3,i+4)
  print(range)
  end_frame$target[end_frame$Position %in% range] = i
}


  





