#export LC_ALL=C
BEGIN { FS=";"
        i = 0
      }
      { 
            a[$4] += $2
            count[i] = $2
            prefix[i] = $4
            row[i] = $1";"$2";"$3";"$4
            i += 1
      }
END   { 
      for (j = 0; j < i; j++) print row[j]";"(count[j]/a[prefix[j]])
      }    