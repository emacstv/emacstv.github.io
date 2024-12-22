import { OrgParser, OrgDocument, OrgHeading } from '../src/index';

describe('OrgParser', () => {
  it('should parse org content into an OrgDocument with OrgHeadings', () => {
    const orgContent = `
* Saturday opening remarks  :emacsconf:emacsconf2024:
:PROPERTIES:
:DATE: 2024-12-07
:MEDIA_URL: https://media.emacsconf.org/2024/emacsconf-2024-sat-open--saturday-opening-remarks--main.webm
:YOUTUBE_URL: https://youtu.be/YrlAfWfgvIQ
:SPEAKERS:
:END:
* Writing academic papers in Org-Roam  :emacsconf:emacsconf2024:org:OrgRoam:academic:research:writing:
:PROPERTIES:
:DATE: 2024-12-07
:URL: https://emacsconf.org/2024/talks/papers
:MEDIA_URL: https://media.emacsconf.org/2024/emacsconf-2024-papers--writing-academic-papers-in-orgroam--vincent-conus--main.webm
:YOUTUBE_URL: https://youtu.be/BKQcgpZS2GM
:TOOBNIX_URL: https://toobnix.org/w/9LYtH8MWCMZ7N4DNteys17
:TRANSCRIPT_URL: https://media.emacsconf.org/2024/emacsconf-2024-papers--writing-academic-papers-in-orgroam--vincent-conus--main.vtt
:SPEAKERS: Vincent Conus
:SERIES: EmacsConf 2024
:END:`;

    const document = OrgParser.parse(orgContent);

    const expectedDocument = new OrgDocument([
      new OrgHeading(
        'Saturday opening remarks',
        ['emacsconf', 'emacsconf2024'],
        {
          DATE: '2024-12-07',
          MEDIA_URL: 'https://media.emacsconf.org/2024/emacsconf-2024-sat-open--saturday-opening-remarks--main.webm',
          YOUTUBE_URL: 'https://youtu.be/YrlAfWfgvIQ',
          SPEAKERS: ''
        },
      ),
      new OrgHeading(
        'Writing academic papers in Org-Roam',
        ['emacsconf', 'emacsconf2024', 'org', 'OrgRoam', 'academic', 'research', 'writing'],
        {
          DATE: '2024-12-07',
          URL: 'https://emacsconf.org/2024/talks/papers',
          MEDIA_URL: 'https://media.emacsconf.org/2024/emacsconf-2024-papers--writing-academic-papers-in-orgroam--vincent-conus--main.webm',
          YOUTUBE_URL: 'https://youtu.be/BKQcgpZS2GM',
          TOOBNIX_URL: 'https://toobnix.org/w/9LYtH8MWCMZ7N4DNteys17',
          TRANSCRIPT_URL: 'https://media.emacsconf.org/2024/emacsconf-2024-papers--writing-academic-papers-in-orgroam--vincent-conus--main.vtt',
          SPEAKERS: 'Vincent Conus',
          SERIES: 'EmacsConf 2024'
        },
      )
    ]);

    expect(document).toEqual(expectedDocument);
  });

  it('should parse org content with multiline description and multilingual titles', () => {
    const orgContent = `
* Chiacchierata Live con Nicola di @NFVblog : Emacs, distro Linux minimali, Gentoo ed altro!
:PROPERTIES:
:YOUTUBE_URL: https://www.youtube.com/watch?v=BrK3rBxMGs0
:DATE:     2023-04-11T19:46:51-07:00
:SPEAKERS: Clarintux: un Tux è per sempre!
:DURATION: 01:06:30
:END:
  Temi trattati nella Live:

  - Emacs: vantaggi rispetto ad altri editor di testo e differenze rispetto a Vim.
  - Distribuzioni minimali vs "full bloated": perché alcuni preferiscono le prime.
  - Panoramica su diverse distro con installazione di un sistema base.
  - Gentoo, tra compilazioni e personalizzazioni.
  - Come mai i programmatori sembrano preferire Linux?
  - I vantaggi di Github/Gitlab anche per non programmatori (repository di dotfiles).

  METTI LIKE E ISCRIVITI AL CANALE 👍

  💻 Il mio Github: https://github.com/clarintux
`;

    const expectedDocument = new OrgDocument([
      new OrgHeading(
        'Chiacchierata Live con Nicola di @NFVblog : Emacs, distro Linux minimali, Gentoo ed altro!',
        [],
        {
          YOUTUBE_URL: 'https://www.youtube.com/watch?v=BrK3rBxMGs0',
          DATE: '2023-04-11T19:46:51-07:00',
          SPEAKERS: 'Clarintux: un Tux è per sempre!',
          DURATION: '01:06:30',
        }
      ),
    ]);

  const document = OrgParser.parse(orgContent);
  expect(document).toEqual(expectedDocument);
  });


  it('should parse org content with EmacsConf2021 example', () => {
    const orgContent = `
* EmacsConf2021 - Emacs Application Framework: A 2021 Update
:PROPERTIES:
:YOUTUBE_URL: https://www.youtube.com/watch?v=bh37zbefZk4
:DATE:     2021-11-28T12:33:02-08:00
:SPEAKERS: Matthew Zeng
:DURATION: 09:14
:END:
  Emacs Application Framework (EAF) is a customizable and extensible GUI application framework that extends Emacs graphical capabilities using PyQt5. There are many new but important updates since EmacsConf2020 last year, this talk will briefly go over them.

  EmacsConf2021 page: https://emacsconf.org/2021/talks/eaf/
  Last year's talk:
  - https://www.youtube.com/watch?v=HK_f8KTuR0s
  - EmacsConf2020 page with Q&A: https://emacsconf.org/2020/talks/34/
  EAF repo: https://github.com/emacs-eaf/emacs-application-framework
`;
    const expectedDocument = new OrgDocument([
      new OrgHeading(
        'EmacsConf2021 - Emacs Application Framework: A 2021 Update',
        [], // No tags
        {
          YOUTUBE_URL: 'https://www.youtube.com/watch?v=bh37zbefZk4',
          DATE: '2021-11-28T12:33:02-08:00',
          SPEAKERS: 'Matthew Zeng',
          DURATION: '09:14',
        }
      ),
    ]);

    const document = OrgParser.parse(orgContent);
    expect(document).toEqual(expectedDocument);
  });


  it('should parse org content with tags and complex properties (SQLite example)', () => {
    const orgContent = `
* Using SQLite as a data source: a framework and an example  :emacsconf:emacsconf2022:db:
:PROPERTIES:
:DATE: 2022-12-03
:URL: https://emacsconf.org/2022/talks/sqlite
:MEDIA_URL: https://media.emacsconf.org/2022/emacsconf-2022-sqlite--using-sqlite-as-a-data-source-a-framework-and-an-example--andrew-hyatt--main.webm
:YOUTUBE_URL: https://youtu.be/mQGhm79f8TY
:TOOBNIX_URL: https://toobnix.org/w/a26FMpRuCKmn4YZU4ysdrd
:TRANSCRIPT_URL: https://media.emacsconf.org/2022/emacsconf-2022-sqlite--using-sqlite-as-a-data-source-a-framework-and-an-example--andrew-hyatt--main.vtt
:SPEAKERS: Andrew Hyatt
:SERIES: EmacsConf 2022
:END:`;

    const expectedDocument = new OrgDocument([
      new OrgHeading(
        'Using SQLite as a data source: a framework and an example',
        ['emacsconf', 'emacsconf2022', 'db'],
        {
          DATE: '2022-12-03',
          URL: 'https://emacsconf.org/2022/talks/sqlite',
          MEDIA_URL: 'https://media.emacsconf.org/2022/emacsconf-2022-sqlite--using-sqlite-as-a-data-source-a-framework-and-an-example--andrew-hyatt--main.webm',
          YOUTUBE_URL: 'https://youtu.be/mQGhm79f8TY',
          TOOBNIX_URL: 'https://toobnix.org/w/a26FMpRuCKmn4YZU4ysdrd',
          TRANSCRIPT_URL: 'https://media.emacsconf.org/2022/emacsconf-2022-sqlite--using-sqlite-as-a-data-source-a-framework-and-an-example--andrew-hyatt--main.vtt',
          SPEAKERS: 'Andrew Hyatt',
          SERIES: 'EmacsConf 2022',
        }
      ),
    ]);

    const document = OrgParser.parse(orgContent);
    expect(document).toEqual(expectedDocument);
  });

  it('should parse org content with colons in title and valid tags', () => {
    const orgContent = `
* emacs : set up config 01 (init file & org file for literate programming) :org:
:PROPERTIES:
:YOUTUBE_URL: https://www.youtube.com/watch?v=HaCIn5gvJ84&feature=youtu.be
:DATE:     2018-10-08T01:43:29-07:00
:SPEAKERS: Aritra Bhattacharjee
:DURATION: 09:45
:END:
  This video is about setting up emacs config file. This is the first part of the series.
  In this video, the init file & org file is set up for literate programming.

  This video describes how to use org-babel to tangle the elisp code to the initialize Emacs.

  Github repository for this setup:
  https://github.com/analyzeninvest/.emacs.d`;

    const expectedDocument = new OrgDocument([
      new OrgHeading(
        'emacs : set up config 01 (init file & org file for literate programming)',
        ['org'],
        {
          YOUTUBE_URL: 'https://www.youtube.com/watch?v=HaCIn5gvJ84&feature=youtu.be',
          DATE: '2018-10-08T01:43:29-07:00',
          SPEAKERS: 'Aritra Bhattacharjee',
          DURATION: '09:45',
        }
      ),
  ]);

  const document = OrgParser.parse(orgContent);
  expect(document).toEqual(expectedDocument);
});
})
;
