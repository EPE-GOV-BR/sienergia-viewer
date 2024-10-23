

fato_produto <- tbl(database$con,"fato_produto")


con

teste <- bench::mark(

  postergando =     tbl(src = database$con, "fato_produto") %>% 
    select(cd_subsetor) %>% 
    distinct() %>% 
    left_join(
      tbl(src = database$con, "dim_subsetor")
    ) %>% 
    select(
      n2_setor, n1_modulo
    ) %>% 
    distinct() %>% 
    collect() 
  

)

