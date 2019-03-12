from os import listdir
import re

# file = open("data/links.yml", "w") 
file = open("data/items.toml", "w") 

# file.write("tiles:\n")
 
# for report in listdir("static/reports"):
    
#     title = re.sub(".*et_al-", "", report)
#     title = re.sub("_[0-9]{6,10}.html", "", title)
#     title = re.sub("_", " ", title)
    
#     file.write("-\n")
#     file.write("  name: '" + title + "'\n")
#     file.write("  url: 'reports/" + report + "'\n")
#     # file.write("\timg: '", img, "'\n")
#     # file.write("  img: 'img.svg'\n")
    
#     file.write("  bg_color: '#005483'\n")
#     file.write("  txt_color: '#ffffff'\n")
#     file.write("  tags: ['music']\n")

# file.close() 

# file.write("tiles:\n")
 
for report in listdir("static/reports"):
    
    authors = re.sub("et_al-.*", "", report)
    authors = re.sub("_", " ", authors)
    
    title = re.sub(".*et_al-", "", report)
    title = re.sub("_[0-9]{6,10}.html", "", title)
    title = re.sub("_", " ", title)
    
    # article_id = title = re.sub("^.*?[0-9]{6,10", "", title)
    # print(article_id)
    
    file.write("[[items]]\n")
    file.write("title = \"" + title + "\"\n")
    file.write("image = \"figures/" + re.sub(".html", "", report) + ".png\"\n")
    file.write("thumb = \"figures/" + re.sub(".html", "", report) + ".png\"\n")
    file.write("description = \"by " + authors + "\"\n")
    # file.write("description = \"abc\"\n")
    file.write("url = \"reports/" + report + "\"\n")
    
    
    # file.write("-\n")
    # file.write("  name: '" + title + "'\n")
    # file.write("  url: 'reports/" + report + "'\n")
    # # file.write("\timg: '", img, "'\n")
    # # file.write("  img: 'img.svg'\n")
    
    # file.write("  bg_color: '#005483'\n")
    # file.write("  txt_color: '#ffffff'\n")
    # file.write("  tags: ['music']\n")

file.close() 