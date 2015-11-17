BEGIN { FS=";"
        # sort by probability
      }
      {     
            summarized[$1] = summarized[$1]"|"$2#"@"$3
      }
END   { for (prefix in summarized) {    
            print prefix";"summarized[prefix]
            }
      }


#                  split(summarized[prefix],s, "|")
 #           asort(s, dest, "@val_num_desc")
  #          joined = dest[1]            
   #         for (i = 2; i <= length(dest); i++)
    #              joined = joined"|"dest[i]