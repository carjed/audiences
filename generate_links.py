from os import listdir
import re

file = open("hugo_data/items.toml", "w")

for report in listdir("output/reports"):

    authors = re.sub("et_al-.*", "", report)
    authors = re.sub("_", " ", authors)

    title = re.sub(".*et_al-", "", report)
    title = re.sub("_[0-9]{6,10}.html", "", title)
    title = re.sub("_", " ", title)

    # shorten title to fit
    if len(title) > 97:
        title = title[0:96]+"..."

    file.write("[[items]]\n")
    file.write("title = \"%s\"\n" % title)
    file.write("image = \"figures/%s.png\"\n" % re.sub(".html", "", report))
    file.write("thumb = \"figures/%s.png\"\n" % re.sub(".html", "", report))
    file.write("description = \"by %s\"\n" % authors)
    # file.write("description = \"abc\"\n")
    file.write("url = \"reports/%s\"\n" % report)

file.close()
