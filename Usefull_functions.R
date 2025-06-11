## Package usuels -----
suppressMessages(library(gplots))
library(ggplot2)
library(knitr)
library(car)
library(ggplot2)
library(grid)
library(missMDA)
library(openxlsx)
suppressMessages(library(dplyr))
library(gtable)
suppressMessages(library(car))
library(scales)
library(FactoMineR)
windowsFonts(Times=windowsFont("TT Times New Roman"))



Save.data<-function(data,name,col=T,row=F,dec=".",sep=";",indic=T,name.comp,unit){
  if(indic==T){
    class.data<-c()
    Nb.var<-dim(data)[2]
    for(i in 1:Nb.var){
      class.data<-c(class.data,class(data[,i]))
      data[,i]<-as.character(data[,i])
    }
    data<-rbind(unit,data)
    data<-rbind(name.comp,data)
    data<-rbind(class.data,data)
  }
  write.table(data,name,col.names=col,row.names = row, dec=dec,sep=sep)
}



Read.data<-function(file,col=T,row=F,dec=".",sep=";",indic=T){
  data.set<-read.table(file,header=col,dec=dec,sep=sep)
  data.set<-data.set[-c(2,3),]
  if(indic==T){
    Nb.var<-dim(data.set)[2]
    for(i in 1:Nb.var){
      data.set[,i]<-as.character(data.set[,i])
    }
    class.data.set<-data.set[1,]
    data.set<-data.set[-1,]
    for(i in 1:Nb.var){
      if(class.data.set[i]=="factor"){
        data.set[,i]<-as.factor(data.set[,i])
      }else if(class.data.set[i]=="numeric"){
        data.set[,i]<-as.numeric(data.set[,i])
      }else if(class.data.set[i]=="integer"){
        data.set[,i]<-as.integer(data.set[,i])
      }else if(class.data.set[i]=="Date"){
        data.set[,i]<-as.Date(data.set[,i])
      }
    }
  }
  return(data.set)
}



Forme.PCA<-function(data,factor.col,col.split,factor.line,prefix,imp=T,imp.scale=T,indiv.centr=0,basal=0){
  #factor col : colonnes que l'on désire mettre en ligne
  #col.split = variables donnant les colonnes
  # factor.line = individu par ligne
  # Indiv.centre  => 0 : valeur brute
  #               => 1 : même moyenne par ligne
  #               => 2 : % d'écart à la moyenne de l'invidivu
  #               => 3 : % d'écart à la sous-moyenne des colonnes précisées via basal (valeurs dans la colonne) 
  indiv<-unique(data[,factor.line])
  ndoub<-length(col.split)
  ncol<-1
  un.col<-list()
  for(i in 1:length(col.split)){
    ncol<-ncol*length(unique(data[,col.split[i]]))
    un.col[[i]]<-(unique(data[,col.split[i]]))
  }
  nb.line<-length(unique(data[,factor.line]))
  un.line<-(unique(data[,factor.line]))
  res<-matrix(ncol=ncol+1,nrow=nb.line,NA)
  res<-as.data.frame(res)
  res[,1]<-as.character(un.line)
  for(i in 1:length(col.split)){
    assign(paste("i",i,sep=""),1)
  }
  multi<-rep(1,ndoub)
  
  if(length(col.split)>1){
    multi[length(col.split)]<-1
    for(i in seq(from=length(col.split)-1,to=1,by=-1)){
      multi[i]<-multi[i+1]*length(unique(data[,col.split[i+1]]))
    }
  }
  nb.line.data<-dim(data)[1]
  for(i in 1:nb.line.data){
    line<-match(data[i,factor.line],res[,1])
    for(j in 1:length(col.split)){
      assign(paste("i",j,sep=""),match(data[i,col.split[j]],un.col[[j]]))
    }
    col<-2
    for(j in seq(from=length(col.split),to=1,by=-1)){
      col<-col+(get(paste("i",j,sep=""))-1)*(multi[j])
    }
    
    res[line,col]<-data[i,factor.col]
  }
  for(i in 1:length(col.split)){
    assign(paste("i",i,sep=""),1)
  }
  k<-2
  while(i1<=length(un.col[[1]])){
    nom<-paste(prefix,as.character(un.col[[1]][i1]),sep="")
    if(length(col.split)>=2){
      for(i in 2:length(col.split)){
        nom<-paste(nom,un.col[[i]][get(paste("i",i,sep=""))],sep=".")
      }
    }
    names(res)[k]<-nom
    k<-k+1
    assign(paste("i",length(col.split),sep=""),get(paste("i",length(col.split),sep=""))+1)
    
    if(length(col.split)>=2){
      for(i in seq(from=length(col.split),to=2,by=-1)){
        if(get(paste("i",i,sep = ""))>length(un.col[[i]])){
          assign(paste("i",i,sep = ""),1)
          assign(paste("i",i-1,sep = ""),get(paste("i",i-1,sep = ""))+1)
        }
      }
    }
  }
  names(res)[1]<-names(data[factor.line])
  na<-F
  i<-2
  row.names(res)<-res[,1]
  ncol<-dim(res)[2]
  while(i<=col){
    if(sum(is.na(res[,i]))>=1){
      na<-T
      i<-col
    }
    i<-i+1
  }
  if(na==T&imp==T){
    res2<-as.data.frame(imputePCA(res[,-1],scale=imp.scale,ncp=(dim(res[,-1])[2]-1))$completeObs)
    res<-cbind(res[,1],res2)
    names(res)[1]<-names(data)[factor.line]
  }
  if(indiv.centr==1){
    sub<-aggregate(data[,factor.col]~data[,factor.line],FUN="mean")
    sub[,2]<-sub[,2]-mean(sub[,2])
    res<-merge(res,sub,by=1)
    for(i in 2:ncol){
      res[,i]<-res[,i]-res[,ncol+1]
    }
    res<-res[,1:ncol]
  }else if(indiv.centr==2){
    sub<-aggregate(data[,factor.col]~data[,factor.line],FUN="mean")
    res<-merge(res,sub,by=1)
    for(i in 2:ncol){
      res[,i]<-(res[,i]-res[,ncol+1])/res[,ncol+1]*100
    }
    res<-res[,1:ncol]
  }else if(indiv.centr==3){
    sub<-aggregate(data[data[,col.split[1]]%in%basal,factor.col]~data[data[,col.split[1]]%in%basal,factor.line],FUN="mean")
    res<-merge(res,sub,by=1)
    for(i in 2:ncol){
      res[,i]<-(res[,i]-res[,ncol+1])/res[,ncol+1]*100
    }
    res<-res[,1:ncol]
  }
  row.names(res)<-res[,1]
  return(res)
}



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



Explo.data<-function(data, col.explo,names=F,col.unique,col.nom=c("V1","V2","V3")){
	# data = Jeux de données
	# col.explo = Numéro des colonnes contenant les valeurs 
	# col.unique = identifiant ligne à conserver
	# names = Faut-il renommer les colonnes pour les vcaleurs à utiliser
	# col.nom => Nom à donner aux nouvelles variables (ID ligne, modalité colonne, valeur mesure)
  Nb.ligne<-dim(data)[1]*length(col.explo)
  Nb.Col<-3
  res<-matrix(nrow=Nb.ligne,ncol=Nb.Col,NA)
  data[,col.unique]<-as.character(data[,col.unique])
  res<-as.data.frame(res)
  if(length(names)>1){
    names<-as.character(names)
  }else{
    names<-names(data)[col.explo]
  }
  res[,3]<-as.numeric(res[,3])
  ligne.data<-dim(data)[1]
  uni<-data[,col.unique]
  for(i in 1:length(uni)){
    sub<-data[data[,col.unique]==uni[i],]
    for(j in 1:length(col.explo)){
      Nb.ligne<-(i-1)*length(col.explo)+j
      res[Nb.ligne,1]<-sub[1,col.unique]
      res[Nb.ligne,2]<-names[j]
      res[Nb.ligne,3]<-sub[1,col.explo[j]]
    }
  }
  k<-1
  res<-res[is.na(res[,3])==F,]
  names(res)<-col.nom
  return(res)
}






AnnotateGraph_bas<-function(data,y.anno,x,y,group,Titre="Pas de titre",scale.shape,delta=5,ydelta=0){
  Pos.X<-which(names(data)==x)
  Pos.Y<-which(names(data)==y)
  Pos.group<-which(names(data)==group)
  data[,Pos.group]<-as.factor(as.character(data[,Pos.group]))
  p<-ggplot(data=data,aes(x=get(x),y=get(y)))+geom_line(aes(linetype=get(group),group=get(group)))+
    geom_point(aes(shape=get(group)),size=1.75)+
    labs(y="Milk Ca Content (mg/kg)",x="Calendar Month")+
    theme_bw()+ #Fond Blanc
    theme(panel.grid = element_blank(), #Supprime les barres verticales
          panel.border = element_blank()) +
    theme(axis.line.x = element_line(color="black", size = 1), #draws x and y axis line
          axis.line.y = element_line(color="black", size = 1))+
    scale_shape_manual(values=scale.shape)+
    theme(axis.text=element_text(family="Times",size=12),legend.position="none",
          axis.title=element_text(family="Times",size=12,face="bold"))+
    theme(axis.line.x = element_line(color="black", size = 1), #draws x and y axis line
          axis.line.y = element_line(color="black", size = 1))+
    #annotate("text",x=1,y=1260,label="A",cex=8)+
    labs(title=Titre) + theme(plot.title = element_text(family="Times",hjust=-0.1,size=17,vjust=-2))
  data$Classement<-rep(NA,dim(data)[1])
  data$sub<-as.factor(data[,Pos.X])
  Res<-data[1,]
  Max<-max(data[,Pos.Y])
  
  if(is.factor(data[,x])==T){
    for (i in 1:length(levels(data[,Pos.X]))){
      sub<-data[data[,Pos.X]==levels(data[,Pos.X])[i],]
      temp<-order(sub[,Pos.Y])
      for(k in 1:length(temp)){
        sub$Classement[temp[k]]<-length(temp)-k+1
      }
      Res<-rbind(Res,sub)
      for(j in 1:dim(sub)[1]){
        p<-p+annotate("text",x=i,y=ydelta+max(sub[,Pos.Y])+delta+(4-sub$Classement[j])*delta,label=sub[,y.anno][j],family="Times")
      }
    }
  }else if(is.numeric(data[,x])==T){
    
    for (i in 1:length(levels(data$sub))){
      sub<-data[data[,Pos.X]==levels(data$sub)[i],]
      temp<-order(sub[,Pos.Y])
      for(k in 1:length(temp)){
        sub$Classement[temp[k]]<-length(temp)-k+1
      }
      Res<-rbind(Res,sub)
      for(j in 1:dim(sub)[1]){
        p<-p+annotate("text",x=sub[1,Pos.X],y=ydelta+max(sub[,Pos.Y])+delta+(4-sub$Classement[j])*delta,label=sub[,y.anno][j],family="Times")
      }
    }
  }
  p<-p+theme(axis.line.x = element_line(color="black", size = 1), #draws x and y axis line
             axis.line.y = element_line(color="black", size = 1))
  Res<-Res[-1,]
  # for(i in 1:length(levels(Res[,Pos.group]))){
  #   sub<-Res[Res[,Pos.group]==levels(Res[,Pos.group])[i],]
  #   for(j in 1:(length(levels(sub[,Pos.X]))-1)){
  #     Pos1<-sub[sub[,Pos.X]==levels(sub[,Pos.X])[j],"Classement"]
  #     Pos2<-sub[sub[,Pos.X]==levels(sub[,Pos.X])[j+1],"Classement"]
  #     p<-p+annotate("segment",x=j+0.3,xend=j+0.7,y=ydelta+Max+delta+(4-Pos1)*delta,yend=ydelta+Max+delta+(4-Pos2)*delta,color="black")
  #   }
  # }
  return(p)
}




Comp2a2<-function(data,comp,x,y,seuil=0.05){
  # On compare les éléments à même niveaux de x
  lvl.x<-unique(data[,x[1]])
  lvl.y<-unique(data[,y[1]])
  if(is.numeric(comp$Adjp)==F){
    comp$Adjp<-as.character(comp$Adjp)
    comp[comp$Adjp=="<.0001","Adjp"]<-"0.0001"
    comp$Adjp<-as.numeric(comp$Adjp)
  }
  
  Res<-rep("a",3)
  for(i in 1:length(lvl.x)){
    sub<-comp[comp[,x[1]]==lvl.x[i]&comp[,x[2]]==lvl.x[i],]
    ligne<-which.max(abs(sub[,"Estimate"]))
    if(sub[ligne,"Estimate"]>0){
      ref<-sub[ligne,y[2]]
    }else{
      ref<-sub[ligne,y[1]]
    }
    temp<-c(as.character(ref),as.character(lvl.x[i]),"a")
    letter=1
    Res<-rbind(Res,temp)
    done<-as.character(ref)
    for(j in 2:(length(lvl.y))){
      if(j!=length(lvl.y)){
        sub1<-sub[!(sub[,y[1]]%in%done)&!(sub[,y[2]]%in%done),]
        ligne<-which.min(abs(sub1[,"Estimate"]))
        if(sub[ligne,"Estimate"]>0){
          ref<-as.character(sub1[ligne,y[2]])
        }else{
          ref<-as.character(sub1[ligne,y[1]])
        }
      }else{
        ref<-as.character(lvl.y[!(lvl.y%in%done)])
      }
      
      
      
      k<-length(done)
      letter.ind<-letter
      while(k >=1){
        p.val<-sub[(sub[,y[1]]==ref&sub[,y[2]]==done[k])|(sub[,y[2]]==ref&sub[,y[1]]==done[k]),"Adjp"]
        if(p.val<seuil){
          if(k==length(done)){
            letter<-letter+1
            letter.ind<-letter
          }else{
            ligne<-Res[Res[,1]==done[k]&Res[,2]==lvl.y]
            last<-dim(Res)[1]
            letter<-letter+1
            letter.ind<-letter
            if(sum(strsplit(Res[last-k+1,3],"*")[[1]]%in%strsplit(Res[last-k,3],"*")[[1]])==length(strsplit(Res[last-k+1,3],"*")[[1]])){
              for(h in ligne:dim(Res)[1]){
                letter.ind<-strsplit(Res[last-k+1,3],"*")[[1]][strsplit(Res[last-k+1,3],"*")[[1]]%in%strsplit(Res[last-k,3],"*")==F]
              }
              Res[last,3]<-paste(Res[last,3],chartr("123456789", "abcdefghi", letter.ind))
            }else{
              
            }
          }
          k<-0
        }else{
          k<-k-1
        }
        temp<-c(as.character(ref),as.character(lvl.x[i]),chartr("123456789", "abcdefghi", letter.ind))
      }
      Res<-rbind(Res,temp)
      done<-c(done,ref)
    }
  }
  Res<-Res[-1,]
  Res<-as.data.frame(Res)
  names(Res)<-c(y[1],x[1],"Comp")
  Res<-merge(data,Res)
  return(Res)
}


facet_nested <- function(rows = NULL, cols = NULL, scales = "fixed", space = "fixed",
                         shrink = TRUE, labeller = "label_value", as.table = TRUE,
                         switch = NULL, drop = TRUE, margins = FALSE, facets = NULL,
                         nest_line = FALSE, resect = unit(0, "mm"), bleed = FALSE)
{
  # Permet de faire des grid sur ggplot2 en précisant ligne et colonnes
  if (!is.null(facets)) {
    rows <- facets
  }
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))
  
  space <- match.arg(space, c("fixed","free_x","free_y","free"))
  space_free <- list(x = any(space %in% c("free_x", "free")),
                     y = any(space %in% c("free_y", "free")))
  
  if (!is.null(switch) && !switch %in% c("both","x","y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }
  
  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions",
         .call = FALSE)
  }
  if (n == 1L) {
    rows <- quos()
    cols <- facets_list[[1]]
  } else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }
  labeller <- ggplot2:::check_labeller(labeller)
  ggproto(NULL, FacetNested, shrink = shrink,
          params = list(
            rows = rows,
            cols = cols,
            margins = margins,
            free = free,
            space_free = space_free,
            labeller = labeller,
            as.table = as.table,
            switch = switch,
            drop = drop,
            nest_line = nest_line,
            resect = resect,
            bleed = bleed
          ))
}

#' @rdname facet_nested
#' @format NULL
#' @usage NULL
#' @export
FacetNested <- ggplot2::ggproto(
  "FacetNested", ggplot2::
    FacetGrid,
  map_data = function(data, layout, params) {
    # Handle empty data
    if (ggplot2:::empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    # Setup variables
    rows <- params$rows
    cols <- params$cols
    
    vars <- c(names(rows), names(cols))
    margin_vars <- list(intersect(names(rows), names(data)),
                        intersect(names(cols), names(data)))
    
    # Add variables
    data <- reshape2::add_margins(data, margin_vars, params$margins)
    facet_vals <- ggplot2:::eval_facets(c(rows, cols), data, params$plot$env)
    
    # Only set as missing if it has no variable in that direction
    missing_facets <- character(0)
    if (!any(names(rows) %in% names(facet_vals))){
      missing_facets <- c(missing_facets, setdiff(names(rows), names(facet_vals)))
    }
    if (!any(names(cols) %in% names(facet_vals))){
      missing_facets <- c(missing_facets, setdiff(names(cols), names(facet_vals)))
    }
    
    # Fill in missing values
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      data <- plyr::unrowname(data[data_rep, , drop = FALSE])
      facet_vals <- plyr::unrowname(
        cbind(facet_vals[data_rep, , drop = FALSE],
              to_add[facet_rep, , drop = FALSE])
      )
    }
    
    # Match columns to facets
    if (nrow(facet_vals) == 0) {
      data$PANEL <- NO_PANEL
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      keys <- plyr::join.keys(facet_vals, layout, by = vars[vars %in% names(facet_vals)])
      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  compute_layout = function(data, params)
  {
    rows <- params$rows
    cols <- params$cols
    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      stop("Facetting variables can only appear in row or cols, not both.\n",
           "Problems: ", paste0(dups, collapse = "'"), call. = FALSE)
    }
    base_rows <- combine_nested_vars(data, params$plot_env, rows, drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) factor(x, levels = rev(ggplot2:::ulevels(x)))
    }
    base_cols <- combine_nested_vars(data, params$plot_env, cols, drop = params$drop)
    base <- ggplot2:::df.grid(base_rows, base_cols)
    base <- reshape2::add_margins(base, list(names(rows), names(cols)), params$margins)
    base <- unique(base)
    panel <- plyr::id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))
    rows <- if (!length(names(rows))) {
      1L
    } else {
      plyr::id(base[names(rows)], drop = TRUE)
    }
    cols <- if (!length(names(cols))) {
      1L
    } else {
      plyr::id(base[names(cols)], drop = TRUE)
    }
    panels <- data.frame(PANEL = panel, ROW = rows, COL = cols,
                         base, check.names = FALSE, stringsAsFactors = FALSE)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL
    panels$SCALE_X <- if (params$free$x) {
      panels$COL
    } else {
      1L
    }
    panels$SCALE_Y <- if (params$free$y) {
      panels$ROW
    } else {
      1L
    }
    panels
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params)
  {
    panel_table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales,
                                         ranges, coord, data, theme, params)
    
    # Setup strips
    col_vars  <- unique(layout[names(params$cols)])
    row_vars  <- unique(layout[names(params$rows)])
    attr(col_vars, "type")  <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type")  <- "rows"
    attr(row_vars, "facet") <- "grid"
    
    # Build strips
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    
    # Merging strips
    merge_cols <- apply(col_vars, 2, function(x) any(rle(x)$lengths > 1))
    merge_rows <- apply(row_vars, 2, function(x) any(rle(x)$lengths > 1))
    
    if (any(merge_cols)) {
      if (switch_x) {
        panel_table <- merge_strips(panel_table, strips$x$bottom,
                                    col_vars, switch_x, params, theme, "x")
      } else {
        panel_table <- merge_strips(panel_table, strips$x$top,
                                    col_vars, switch_x, params, theme, "x")
      }
    }
    
    if (any(merge_rows)) {
      if (switch_y) {
        panel_table <- merge_strips(panel_table, strips$y$left,
                                    row_vars, switch_y, params, theme, "y")
      } else {
        panel_table <- merge_strips(panel_table, strips$y$right,
                                    row_vars, switch_y, params, theme, "y")
      }
    }
    panel_table
  }
)

# Helper functions -----------------------------------------------

combine_nested_vars <- function(data, env = emptyenv(), vars = NULL, drop = TRUE) {
  if (length(vars) == 0)
    return(data.frame())
  values <- ggplot2:::compact(plyr::llply(data, ggplot2:::eval_facets, facets = vars,
                                          env = env))
  has_all <- unlist(lapply(values, length)) == length(vars)
  if (!any(has_all)) {
    missing <- lapply(values, function(x) setdiff(names(vars), names(x)))
    missing_txt <- vapply(missing, ggplot2:::var_list, character(1))
    name <- c("Plot", paste0("Layer ", seq_len(length(data) - 1)))
    stop("At least one layer must contain all faceting variables: ",
         ggplot2:::var_list(names(vars)), ".\n", paste0("* ", name, " is missing ",
                                                        missing_txt, collapse = "\n"),
         call. = FALSE)
  }
  base <- unique(plyr::ldply(values[has_all]))
  if (!drop) {
    base <- ggplot2:::unique_combs(base)
  }
  for (value in values[!has_all]) {
    if (ggplot2:::empty(value))
      next
    old <- base[setdiff(names(base), names(value))]
    new <- unique(value[intersect(names(base), names(value))])
    if (drop) {
      new <- ggplot2:::unique_combs(new)
    }
    old[setdiff(names(base), names(value))] <- rep("", nrow(old))
    base <- rbind(base, ggplot2:::df.grid(old, new))
  }
  if (ggplot2:::empty(base)) {
    stop("Facetting variables must have at least one value",
         call. = FALSE)
  }
  base
}


merge_strips <- function(panel_table, strip, vars, switch, params, theme, orient = c("x","y"))
{
  if (is.null(strip)) {
    return(panel_table)
  }
  n_levels <- nrow(strip[[1]]$layout)
  splitstrip <- lapply(seq_len(n_levels), function(i) {
    switch(orient,
           x = lapply(strip, function(x) x[i, ]),
           y = lapply(strip, function(x) x[, i]))
    
  })
  
  if (params$bleed) {
    merge <- apply(vars, 2, function(x) any(rle(x)$lengths > 1))
  } else {
    merge <- sapply(1:ncol(vars), function(i){
      x <- apply(subset.data.frame(vars, select = seq(i)), 1, paste0, collapse = "")
      return(any(rle(x)$lengths > 1))
    })
  }
  
  if (orient == "y" && !switch) {
    vars <- rev(vars)
    merge <- rev(merge)
  }
  if (orient == "x" && switch) {
    vars <- rev(vars)
    merge <- rev(merge)
    splitstrip <- rev(splitstrip)
  }
  
  sizes <- switch(orient,
                  x = do.call(unit.c, lapply(splitstrip, max_height)),
                  y = do.call(unit.c, lapply(splitstrip, max_width)))
  
  assign("panel_table", panel_table, 1)
  
  grabwhat <- switch(orient,
                     x = grepl("strip-t|strip-b", panel_table$layout$name),
                     y = grepl("strip-r|strip-l", panel_table$layout$name))
  
  pos_y <- unique(panel_table$layout$t[grabwhat])
  pos_x <- unique(panel_table$layout$l[grabwhat])
  panel_pos <- find_panel(panel_table)
  
  if (orient == "x") {
    nudge <- if (pos_y < panel_pos$t) -1 else -1
    panel_table <- panel_table[-pos_y,]
    panel_table <- gtable_add_rows(panel_table, sizes, pos_y + nudge)
    
  } else {
    nudge <- if (pos_x < panel_pos$l) -1 else 0
    panel_table <- panel_table[, -pos_x]
    panel_table <- gtable_add_cols(panel_table, sizes, pos_x + nudge)
  }
  
  for(i in seq_len(n_levels)) {
    if (!merge[i]) {
      panel_table <- gtable_add_grob(
        panel_table, splitstrip[[i]],
        t = pos_y + switch(orient, x = i + nudge, y = 0),
        l = pos_x + switch(orient, x = 0, y = i + nudge),
        z = 2, clip = "on",
        name = paste0("strip-", orient, "-", seq_along(splitstrip[[i]]))
      )
    } else {
      j <- as.numeric(as.factor(vars[,i]))
      ends <- cumsum(rle(j)$lengths)
      starts <- c(1, which(diff(j) != 0) + 1)
      panel_table <- gtable_add_grob(
        panel_table, splitstrip[[i]][starts],
        t = switch(orient, x = pos_y + i + nudge, y = pos_y[starts]),
        b = switch(orient, x = pos_y + i + nudge, y = pos_y[ends]),
        l = switch(orient, x = pos_x[starts], y = pos_x + i + nudge),
        r = switch(orient, x = pos_x[ends],   y = pos_x + i + nudge),
        z = 2, clip = "on",
        name = paste0("strip-", orient, "-", seq_along(splitstrip[[i]][starts]))
      )
      
      if(params$nest_line && any(starts != ends)) {
        insert_here <- which(starts != ends)
        indicator <- linesGrob(
          x = switch(orient,
                     x = unit(c(0, 1), "npc") + c(1, -1) * params$resect,
                     y = if (switch) c(1, 1) else c(0, 0)),
          y = switch(orient,
                     x = if (switch) c(1, 1) else c(0, 0),
                     y = unit(c(0, 1), "npc") + c(1, -1) * params$resect),
          gp = grid::gpar(col = theme$line$colour,
                          lty = theme$line$linetype,
                          lwd = theme$line$size * .pt,
                          lineend = theme$line$lineend))
        panel_table <- gtable_add_grob(
          panel_table, lapply(seq_along(insert_here), function(x) indicator),
          t = switch(orient, x = pos_y + i + nudge,
                     y = pos_y[starts[insert_here]]),
          b = switch(orient, x = pos_y + i + nudge,
                     y = pos_y[ends[insert_here]]),
          l = switch(orient, x = pos_x[starts[insert_here]],
                     y = pos_x + i + nudge),
          r = switch(orient, x = pos_x[ends[insert_here]],
                     y = pos_x + i + nudge),
          z = 3, clip = "on",
          name = "nesting-indicator"
        )
      }
    }
  }
  panel_table
}
