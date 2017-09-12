# function to extract the logic rules of a given model if you supply one of the leaf prediction values
rpart_logic_extraction <- function(model, y_val_pred) {
  
  tree_info = model$frame %>%
  select(-ncompete, -nsurrogate, -wt, -complexity) %>%
  mutate(depth = rpart:::tree.depth(as.numeric(rownames(.)))) %>% # add depth for nodes
  mutate(label = labels(model, digits = getOption("digits"), minlength = 0L)) %>% # add labels
  mutate(terminal_node = ifelse(var == "<leaf>", 1, 0)) %>% # flag for terminal nodes (leafs)
  mutate(yval_check = yval == y_val_pred) %>% # flag for match with node
  .[1:which(.$yval_check == 1),] # drop all rows below match
  
  # need to extract up the tree
  current_depth = tree_info$depth[nrow(tree_info)]  
  depth_index = tree_info$depth
  row_required = c(length(depth_index))
  for (i in current_depth:1) {
    row = max(which(depth_index < i))
    row_required = append(row_required, row)
    depth_index = depth_index[1:row]
  }
  
  string = paste(tree_info[sort(row_required),]$label, collapse = " >> ")
  
  return(string)
}

# similar to the regression extraction function but is used to extract it for a classification tree
# will extract the rules for all related branches which result in the supplied class
rpart_logic_extraction_class <- function(model, y_val_pred) {
  
  class_num = which(LETTERS %in% y_val_pred)
  
  tree_info = model$frame %>%
    select(-ncompete, -nsurrogate, -wt, -complexity, -yval2) %>%
    mutate(depth = rpart:::tree.depth(as.numeric(rownames(.)))) %>% # add depth for nodes
    mutate(label = labels(model, digits = getOption("digits"), minlength = 0L)) %>% # add labels
    mutate(terminal_node = ifelse(var == "<leaf>", 1, 0))  %>%
    mutate(yval_check = yval == class_num & terminal_node == 1)
      
  node_pos = which(tree_info$yval_check == TRUE)
  
  # need to loop the extracting up the tree
  extract_up_tree <- function(df, which) {
    df = df[1:which,]
    current_depth = df$depth[nrow(df)]  
    depth_index = df$depth
    row_required = c(length(depth_index))
    for (i in current_depth:1) {
      row = max(which(depth_index < i))
      row_required = append(row_required, row)
      depth_index = depth_index[1:row]
    }
    string = paste(df[sort(row_required),]$label, collapse = " >> ")
    return(string)
  }
  
  # need to extract up the tree
  rules = sapply(node_pos, function(x){extract_up_tree(tree_info, x)})
  return(rules)
  
}

# These functions are for use with the rpart.plot package, these are used to change the look
# node.fun1 is a function which will show a class label for a regression tree instead of its value (requires a lkup in env)
node.fun1 <- function(x, labs, digits, varlen) {
  x$frame$yval <- round(x$frame$yval, 4)
  x$frame <- left_join(x$frame, lkup, by = "yval") %>%
    mutate(perc = as.character(round(n/max(n)*100))) %>%
    mutate(value_label = ifelse(is.na(value_label), "", value_label)) %>%
    mutate(value_label = gsub(".*_(.{1})", "\\1", value_label))
  
  labs = paste0(x$frame$perc, "%\n", x$frame$value_label)
  labs[x$frame$var != "<leaf>"] <- gsub("\\n", "", labs[x$frame$var != "<leaf>"])
  labs
}

# node.fun2 is a function which maps certain nodes to colours based on a value label and lookup
node.fun2 <- function(x) {
  colz = data.frame(value_label = c("", "A", "B", "C", "D"),
                    col = c("#e6e6e6", "#ff5252", "#ffa852", "#ffe552", "#d9ff52"))
  
  x$frame$yval <- round(x$frame$yval, 4)
  x$frame <- left_join(x$frame, lkup, by = "yval") %>%
    mutate(value_label = ifelse(is.na(value_label), "", value_label)) %>%
    mutate(value_label = gsub(".*_(.{1})", "\\1", value_label)) %>%
    left_join(colz, by = "value_label")
  x$frame$col
}

