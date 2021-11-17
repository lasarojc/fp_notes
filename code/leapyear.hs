bissexto x = if mod x 4 /= 0 
                then False
                else if mod x 100 /= 0 
                    then True
                    else if mod x 400 == 0 
                        then True
                        else False
