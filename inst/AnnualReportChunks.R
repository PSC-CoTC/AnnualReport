#=======================================================================================================
# Code Chunks for CoTC annual report
# AnnualReportChunks,R
# Mike O'Brien
# Created: 2020-04


#=======================================================================================================
## @knitr firsttable

t1 <- annual.tbl.second %>%
  mutate_if(colnames(.) %in% c("pre.cap", "pre.model", "post.cap", "post.estd", "pre.escapement", "post.escapement", "pre.ocean.age", "post.ocean.age"), ~as.double(as.character(.))) %>%
  mutate_if(colnames(.) %in% c("pre.cap", "pre.model", "post.cap", "post.estd"),  ~percent(.,accuracy = 0.1))


table1form <- kable(t1, format = "latex", digits = c(0), format.args = list(big.mark =","), escape = T,
                    col.names = c("Managment unit", "Status", "Cap", "Model", "Status",  "Cap", "Estd", "Pre", "Post", "Pre", "Post"),
                    caption = 'Table 1: Summary of the 2018 return of natural spawning Coho Management Units.
                    Total exploitation rate (ER) represents the sum of US and Canadian exploitation rates on each
                    Management Unit (MU). Status (i.e., "L"ow, "M"oderate, "A"bundant) is based on cohort abundance.
                    Pre-season status is based on agency forecasts of MU cohort abundance and post-season status is
                    based on backwards Coho FRAM run reconstruction of cohort abundances.
                    The ER Cap on the Interior Fraser MU is 20% due to its Low MU status.
                    ER Caps are not available for other Canadian MUs. ER Caps on each of the Inside US MUs
                    (Skagit, Stillaguamish, Snohomish, Hood Canal, and US Strait JDF) are based on MU status,
                    which is determined by cohort abundance. ER Caps on each of the Outside US MUs
                    (Quillayute Fall, Queets, Hoh, Grays Harbor) are based on cohort abundance and the escapement goals
                    for each MU. Outside US MUs are managed for an escapement range; the floor of the escapement range
                    is used for MU status determination. Pre-season modelled ERs are the result of US and Canadian fishery
                    planning processes. Post-season estimated ERs are generated from backwards Coho FRAM.
                    Cohort abundance is the ocean age-3 abundance and represents escapement plus fishery mortality,
                    but not natural mortality.') %>%
  # kable_styling(latex_options = "striped") %>%
  column_spec(2, border_left = TRUE)%>%
  column_spec(5, border_left = TRUE)%>%
  column_spec(8, border_left = TRUE)%>%
  column_spec(10, border_left = TRUE)%>%
  column_spec(11, border_right = TRUE) %>%
  add_header_above(c(" ", "Pre-Season total ER" = 3, "Post-Season total ER" = 3, "Escapement" = 2, "Recruitment\newline (Ocean Age-3)" = 2)) %>%
  landscape()
# add_header_above(c(" ", "Total Exploitation Rate" = 6, " ", " " = 3))

print(table1form)


##@knitr secondtable


t2 <- annual.tbl.first %>%
  select(-cap.method) %>%
  mutate_if(is.numeric, ~percent(.,accuracy = 0.1)) %>%
  mutate(us.pre.cap = if_else(management.unit %in% c("Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF", "Quillayute", "Hoh", "Queets", "Grays Harbor"), paste(us.pre.cap, footnote_marker_symbol(2), sep = " "), us.pre.cap )) %>%
  mutate(us.post.cap = if_else(management.unit %in% c("Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF", "Quillayute", "Hoh", "Queets", "Grays Harbor"), paste(us.post.cap, footnote_marker_symbol(2), sep = " "), us.post.cap )) %>%
  mutate(cdn.pre.cap = if_else(management.unit == "Interior Fraser", paste(cdn.pre.cap, footnote_marker_symbol(2), sep = " "), cdn.pre.cap)) %>%
  mutate(cdn.post.cap = if_else(management.unit == "Interior Fraser", paste(cdn.post.cap, footnote_marker_symbol(2), sep = " "), cdn.post.cap))



# t2$us.pre.cap[5:13] <-  (paste0(t2$us.pre.cap[5:13], footnote_marker_symbol(2)))
# t2$us.post.cap[5:13] <- (paste0(t2$us.post.cap[5:13], footnote_marker_symbol(2)))
# t2$cdn.pre.cap[2] <- (paste0(t2$cdn.pre.cap[2], footnote_marker_symbol(2)))
# t2$cdn.post.cap[2] <- (paste0(t2$cdn.pre.cap[2], footnote_marker_symbol(2)))

cdn_skag_unused <- t2$cdn.pre.unused[t2$management.unit == "Skagit"]
us_skag_cap <- t2$us.pre.cap[t2$management.unit == "Skagit"] %>%
  str_sub(1, str_locate(., "%")[1])

footnote2 <- str_glue("Contains the base cap ER plus the unused portion of the intercepting party's ER. For example in the preseason, Skagit includes US share plus the unused {cdn_skag_unused} Canadian allocation, totalling {us_skag_cap} total US allocation.")

table2form <-  kable(t2, digits = 1,
                     col.names = c("Managment unit", "Cap", "Model", "Unused", "Cap", "Estd", "Unused", "Cap", "Model", "Unused", "Cap", "Estd","Unused"), escape = F) %>%
  # kable_styling("striped") %>%
  column_spec(2, border_left = TRUE)%>%
  column_spec(5, border_left = TRUE)%>%
  column_spec(8, border_left = TRUE)%>%
  column_spec(11, border_left = TRUE)%>%
  column_spec(13, border_right  = TRUE)%>%
  add_header_above(c(" ", "Pre-Season" = 3, "Post-Season" = 3, "Pre-season" = 3, "Post-Season" = 3))%>%
  add_header_above(c(" ", "Southern U.S.* Exploitation Rate" = 6, "Canadian Exploitation Rate" = 6)) %>%
  footnote(symbol = c("Southern U.S. is limited to US fisheries south of the Canadian border.", footnote2)) %>%
  landscape()

print(table2form)

##@knitr thirdtable

t3_per <- annual.tbl.third %>%
  filter(psc.fishery.name %notin% c("escapement", "cohort")) %>%
  mutate_if(is.numeric, ~percent(.,accuracy = 0.1))

t3_cnt <- annual.tbl.third %>%
  filter(psc.fishery.name %in% c("escapement", "cohort")) %>%
  mutate_if(is.numeric, ~comma(.))
# mutate_if(is.numeric, round) %>%
#
# mutate_if(is.numeric, as.character)
t3_cnt_GS <- select(t3_cnt, c("psc.fishery.name", starts_with("Georgia")))

t3_cnt_GS[1,2:ncol(t3_cnt_GS)] <- paste0(t3_cnt_GS[1,2:ncol(t3_cnt_GS)],footnote_marker_symbol(3))
t3_cnt_GS[2,2:ncol(t3_cnt_GS)] <- paste0(t3_cnt_GS[2,2:ncol(t3_cnt_GS)],footnote_marker_symbol(2))

t3_cnt_oth <- select(t3_cnt, -starts_with("Georgia")) %>%
  mutate(`Lower Fraser` = if_else(psc.fishery.name == "escapement",
                                  paste0(`Lower Fraser`, footnote_marker_symbol(1)," "),
                                  paste0(`Lower Fraser`, footnote_marker_symbol(2))))

t3_cnt <- left_join(t3_cnt_GS, t3_cnt_oth, "psc.fishery.name")


t3 <- bind_rows(t3_per, t3_cnt) %>%
  rename(`Fishery Name` = "psc.fishery.name")


table3form <-  kable(t3, escape = F) %>%
  # kable_styling("striped") %>%
  column_spec(1, bold = T) %>%
  row_spec(c(13, 23, 25), background = "#fcf8e3") %>%
  footnote(symbol = c("Modelled values of terminal run size occuring after lower Fraser in river fisheries", " Modelled values of terminal run size that are not directly estimated", "Modelled values that are not directly estimated"))%>%
  landscape()


print(table3form)
