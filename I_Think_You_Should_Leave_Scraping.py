from playwright.sync_api import sync_playwright, Playwright
import re
import pandas as pd
import time

pw = sync_playwright().start()

chrome = pw.chromium.launch(headless=False)

page = chrome.new_page()

page.goto("https://www.imdb.com/title/tt10050772/reviews/?ref_=ttrt_sa_3")

page_count = 1

while page_count != 0:
    page.get_by_role("button", name="Load More").click(timeout=1000000)
    time.sleep(1)

    page_count = page.get_by_role("button", name="Load More").count()
    print(page_count)
    




link_count = page.locator("css=.lister-item-content").count()

comment_links = []*link_count

for i in range(link_count):
    comment = page.locator("css=.lister-item-content").nth(i).inner_text()
    comment_df = pd.DataFrame({'comment': [comment]})
    comment_links.append(comment_df)

all_comments = pd.concat(comment_links)

all_comments.to_csv("ah_shizzle.csv")

chrome.close()

pw.stop()
