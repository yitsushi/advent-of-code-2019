import click
import os
import os.path
from collections import namedtuple
from jinja2 import Environment

ProjectFile = namedtuple('ProjectFile', ['Name', 'Content'])
Project = namedtuple('Project', ['Name', 'Structure'])


def readProjectFile(file):
    with open(f'template/{file}.j2', 'r') as f:
        return ProjectFile(file, f.read())


def projectStructure():
    return [
        readProjectFile('Main.hs'),
        readProjectFile('Lib.hs'),
        readProjectFile('Part1.hs'),
        readProjectFile('Part2.hs'),
    ]


def readProject(day):
    return Project(f'Day{day}', projectStructure())


def generateProjectFile(file, day):
    return Environment().from_string(file.Content).render(day=day)


@click.command()
@click.option('--day', prompt='Day', type=int)
def app(day):
    paddedDay = f'{day:02d}'

    project = readProject(paddedDay)
    if os.path.isdir(f'src/{project.Name}'):
        print(" -- Already exists!")
        return
    os.mkdir(f'src/{project.Name}')

    for file in project.Structure:
        print(f' -- {project.Name}/{file.Name}')
        with open(f'src/{project.Name}/{file.Name}', 'w') as f:
            f.write(generateProjectFile(file, paddedDay))

    print(" -- Done. Happy Coding ;)")


if __name__ == '__main__':
    app()
