name: News monitor

permissions:
  contents: write

on:
  workflow_dispatch:
    inputs:
      date_from:
        description: "YYYY-MM-DD"
        required: false
      date_to:
        description: "YYYY-MM-DD"
        required: false
  schedule:
    - cron: '0 * * * *'   # každou hodinu

jobs:
  run:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      # 🕒 kontrola času (jen jednou)
      - name: Check Prague time
        id: time
        run: |
          H=$(TZ=Europe/Prague date +%H)
          echo "hour=$H" >> $GITHUB_OUTPUT
          echo "Hour in Prague: $H"

      # 🚫 skip mimo 03/15 (ALE ne při ručním spuštění)
      - name: Skip run outside schedule
        if: steps.time.outputs.hour != '03' && steps.time.outputs.hour != '15' && github.event_name != 'workflow_dispatch'
        run: |
          echo "Skipping run (not 03/15 Prague)"
          exit 0

      # 🟢 odtud už běží všechno vždy konzistentně
      - name: Setup R
        uses: r-lib/actions/setup-r@v2

      - name: Install system libs
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev

      - name: Install R packages
        run: |
          Rscript -e 'install.packages(c(
            "tidyRSS","dplyr","purrr","stringr",
            "readr","tibble","xml2","tidyr","rvest"
          ), repos="https://cloud.r-project.org")'

      - name: Verify packages
        run: |
          Rscript -e 'print("Installed packages:"); print(installed.packages()[,1])'

      # ▶️ spuštění skriptu
      - name: Run script
        run: |
          Rscript source.R "${{ github.event.inputs.date_from }}" "${{ github.event.inputs.date_to }}"

      # 🔍 debug – uvidíš, co vzniklo
      - name: Show files
        run: |
          echo "Files:"
          ls -la

      # 💾 commit výsledků
      - name: Commit results
        run: |
          git config user.name "github-actions"
          git config user.email "github-actions@github.com"

          git add source.csv news_monitor.log

          git diff --cached --quiet || git commit -m "Auto update news output"

          git push
