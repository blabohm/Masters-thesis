################################################################################
# DETOUR INDEX 6 LOCAL SIGNIFICANCE TOOL
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################
# DESCRIPTION
# This tool is designed to calculate two indices that can be used to assess the
# accessibility of public green spaces (GS) in urban areas. The first index goes
# by the name of detour index (DI) and puts the network distance and the
# euclidean distance from a residential building entry to the nearest GS entries
# into relation. It can assume values between 0 and 1. The closer to 1 the
# value, the more direct the way to the next public green spaces is. If the DI
# is closer to 0, the inhabitants of the building have to take larger detours
# to reach the next GS.
# The local significance index (LS) takes into account the area of a green
# space, the network distance from building entry to GS entry and the
# number of persons inhabiting the building. The LS values are higher, the
# larger the GS area, the higher the number of people accessing it and the
# closer the network distance between building and GS entries.
# Together, these indices can be used to assess where inhabitants of a city have
# barriers on their way to public green spaces, and which GS have a potential to
# be overused.
#
################################################################################
# INPUT DATA
# - urban atlas (UA) data of a cities FUA (including population values for
#   residential areas)
# - polygon of the area of interest (if not provided, UA core will be used)
# - URAU code (?)
################################################################################
# MODULE 1 - OSM DOWNLOAD
# Download OpenStreetMap (OSM) data covering the city polygon
#
################################################################################
# MODULE 2 - DATA PREPARATION
# 2.1 - NETWORK CLEANING
#
# 2.2 - BUILDING PREPARATION
#
# 2.3 - GREEN SPACE ENTRY DETECTION
#
# 2.4 - NETWORK BLENDING
#
################################################################################
# MODULE 3 - INDEX BUILDING
#
################################################################################