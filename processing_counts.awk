BEGIN { }
      NR>1{ split($0, term, "\"")
      	split(term[2], a, " ")
      	split(term[3], count, " ")
            


            if (length(a)==4)
                  prefix = a[1]" "a[2]" "a[3]
            else if (length(a)==3)    
                  prefix = a[1]" "a[2]
            else if (length(a)==2)  
                  prefix = a[1]
            else   
                  prefix = ""

            print term[2]";" count[1]";"length(a)";"prefix

      }
END   { }