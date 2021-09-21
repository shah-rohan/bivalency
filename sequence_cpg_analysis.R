library(tidyverse)
library(stringi)

sequence_data = read.table("mm10_promoters_sequences_500up_2500down.txt", stringsAsFactors = F)
colnames(sequence_data) = c("Gene", "sequence")
sequence_data$class = "ICP"

for (i in 1:length(sequence_data$sequence))
{
  seqtxt = sequence_data$sequence[i]
  seqlen = nchar(seqtxt)
  
  hcpstat = F
  icpstat = F
  
  for (j in 0:(seqlen - 500))
  {
    seqstart = 1+j
    seqstop = 500+j
    
    subseq = substr(seqtxt, seqstart, seqstop)
    cpg_count = stri_count(subseq, fixed = "CG")
    c_count = stri_count(subseq, fixed = "C")
    g_count = stri_count(subseq, fixed = "G")
    
    gcfrac = (c_count + g_count)/500
    cpg_oe = cpg_count*500/(c_count*g_count)
    
    if (c_count == 0 | g_count == 0)
    {
      next
    }
    
    if (gcfrac >= 0.55 & cpg_oe >=0.6)
    {
      hcpstat = T
      break
    }
    
    if (cpg_oe >= 0.4)
    {
      icpstat = T
    }
    
  }
  
  if (hcpstat) { sequence_data$class[i] = "HCP" } else if (!icpstat) { sequence_data$class[i] = "LCP" }
  print(i)
}