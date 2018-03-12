# trying out network visualisation and analysis
# now fully realised in HIWWT Analysis :)

require(igraph)


# brent network
# only want brent movements with known origin and destination
# where both sites are strategy locations

bnet <- subset(hiwwt, Species=='DB' &
               Oknown=='Yes' & Dknown == 'Yes' &
                 FieldID_Origin %in% sites & Destination_code %in% sites &
                 grepl('MO', RecordID) & Count > 0 &
                 FieldID_Origin != Destination_code)

bnet_edges <- subset(bnet, select=c(FieldID_Origin, Destination_code))

dolphin <- graph_from_data_frame(bnet_edges, directed=F)
plot(dolphin, main='Brent Network')



# wader network
# only want wader movements with known origin and destination
# where both sites are strategy locations

wnet <- subset(hiwwt, Species!='DB' &
                 Oknown=='Yes' & Dknown == 'Yes' &
                 FieldID_Origin %in% sites & Destination_code %in% sites &
                 grepl('MO', RecordID) & Count > 0 &
                 FieldID_Origin != Destination_code)

wnet_edges <- subset(wnet, select=c(FieldID_Origin, Destination_code))

sharks <- graph_from_data_frame(wnet_edges, directed=F))
plot(sharks, main='Wader Network')
