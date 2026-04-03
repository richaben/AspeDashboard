FROM rocker/r-ver:4.4.2

WORKDIR /project

# 1. Installer dépendances système Ubuntu (libcurl, sf, etc.)
RUN apt-get update && apt-get install -y \
    # Pour curl, httr, etc.
    libcurl4-openssl-dev \
    libssl-dev \
    # Pour sf (spatial)
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    # Pour xml2
    libxml2-dev \
    # Pour ragg (fonts)
    libharfbuzz-dev \
    libfribidi-dev \
    # Nettoyer pour réduire taille image
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean
    
# Copier les fichiers renv et dépendances
COPY pak_packages.txt ./
    
    # Installer pak si pas présent, puis installer les packages
    RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')" \
&& R -e "pak::pkg_install(readLines('pak_packages.txt'))"

CMD ["R"]
