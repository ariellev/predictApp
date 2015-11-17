BEGIN { FS=";"}
      {  	split($1, a, " ")
      	
       	if (length(a)==4)
       		print a[1]" "a[2]" "a[3]";"a[4]";"$5
       	else if (length(a)==3)	  
       		print a[1]" "a[2]";"a[3]";"$5
       	else if (length(a)==2)	
       		print a[1]";"a[2]";"$5
       	else   
       		print ";"a[1]";"$5
      }
END   { }

