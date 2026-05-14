# In the real build, gptl would be a separate library. Here, for simplicity, we add the
# stub version of gptl to the share library.
list(APPEND share_sources
  gptl.F90)

sourcelist_to_parent(share_sources)
