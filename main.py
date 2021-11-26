# dj mag top 100 디제이 사진 크롤

#from urllib.request import urlopen
from bs4 import BeautifulSoup
import urllib.request
from urllib.error import URLError, HTTPError
import csv

unique_djs = []
imgs = []

def openurl(url, year):
        try:
                #403 방지
                headers = {'User-Agent':'chrome/66.0.3359.181'}
                req = urllib.request.Request(url, headers=headers)
                html = urllib.request.urlopen(req)

        except HTTPError as e:
                err = e.read()
                code = e.getcode()

        source = html.read()
        html.close()

        bsObject = BeautifulSoup(source, "html.parser")
        figures = bsObject.body.find_all('figure', {'class':'media--image'})

        return extract_imgs(figures, year)

def extract_imgs(figures, year):
        for figure in figures:
                #print(figure.find('a')['href'])
                #print(figure.find('img')['src'])
                dj = ''
                # dj 이름 저장된 방식이 연도별로 달라서 케이스를 나눠야함
                if figure.find('a')['href'][:8] == '/top-100':
                        dj = figure.find('a')['href'][23:].replace("-", " ")
                elif figure.find('a')['href'][:13] == '/content/poll':
                        dj = figure.find('a')['href'][19:].replace("-", " ")
                elif figure.find('a')['href'][:9] == '/content/':
                         dj = figure.find('a')['href'][9:].replace("-", " ")
                img_src = figure.find('img')['src']
                if dj not in unique_djs:
                        imgs.append([dj, img_src])

        return imgs

def save_imgs(imgs):
        header = ['dj', 'img']
        with open('results/dj_imgs.csv', 'w', encoding='UTF8', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(header)
            writer.writerows(imgs)

if __name__=="__main__":
        year = 2021
        while year > 2003:
                openurl("https://djmag.com/top100dj?year={}".format(year), year)
                year -= 1
        save_imgs(imgs)